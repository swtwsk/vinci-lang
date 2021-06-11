{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE TupleSections #-}
module CPS.CoreToCPS (coreToCPS, coreProgToCPS) where

import Control.Monad.Identity
import Control.Monad.Reader
import Data.Bifunctor (second)
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Utils.Tuple
import Utils.VarSupply (VarSupply, evalVarSupply, nextVar)

import qualified Core.AST as Core
import Core.CoreManager (CoreManager(..))
import Core.Ops
import Core.Types as Core (Type(..))
import Core.Utils (aggregateApplications)
import qualified CPS.AST as CPS
import ManglingPrefixes (coreToCPSContPrefix, coreToCPSVarPrefix)
import StructDefMap (StructDefMap)

type TranslateM = ReaderT (StructDefMap Core.Type) (VarSupply (String, String))
type CCont = CPS.Var -> TranslateM CPS.CExpr

coreToCPS :: CoreManager Identity -> ([CPS.CFunDef], StructDefMap CPS.CType)
coreToCPS cm = (runMonad (mapM coreToCPS' $ _progs cm), cStructDefs)
    where
        runMonad = flip evalVarSupply supp . flip runReaderT (_structDefs cm)
        cStructDefs = Map.map (second coreTypeTranslation <$>) (_structDefs cm)

coreProgToCPS :: Core.Prog Identity -> CPS.CFunDef
coreProgToCPS prog = evalVarSupply (runReaderT (coreToCPS' prog) Map.empty) supp

supp :: [(String, String)]        
supp = [(coreToCPSContPrefix ++ show x, coreToCPSVarPrefix ++ show x) 
                                                        | x <- [(0 :: Int) ..]]

coreToCPS' :: Core.Prog Identity -> TranslateM CPS.CFunDef
coreToCPS' (Core.Prog progName args expr) = do
    (kName, _) <- nextVar
    expr' <- coreExprToCPSWithCont expr kName
    let cpsPName = coreVarTranslation progName
        cpsArgs = coreVarTranslation <$> args
    return $ CPS.CFunDef cpsPName kName cpsArgs expr'

coreExprToCPS :: Core.Expr Identity -> CCont -> TranslateM CPS.CExpr
coreExprToCPS (Core.Var x) k = k (coreVarTranslation x)
coreExprToCPS e@Core.App {} k = do
    (kName, xName) <- nextVar
    let (f, args) = aggregateApplications e
    coreExprToCPS f $ \fn@(CPS.Var _ ft) -> do
        e' <- coreExprRec fn args [] kName
        let fRetT = CPS.resType ft
            x' = CPS.Var xName fRetT
        kApplied <- k x'
        return $ CPS.CLetCont kName x' kApplied e'
coreExprToCPS (Core.Let (Core.VarId n t) e1 e2) k = do
    (jName, _) <- nextVar
    e1' <- coreExprToCPSWithCont e1 jName
    e2' <- coreExprToCPS e2 k
    let t' = coreTypeTranslation (runIdentity t)
    return $ CPS.CLetCont jName (CPS.Var n t') e2' e1'
coreExprToCPS (Core.If cond e1 e2) k = do
    (k0, x0) <- nextVar
    (k1, x1) <- nextVar
    (k2, x2) <- nextVar
    e1' <- coreExprToCPSWithCont e1 k0
    e2' <- coreExprToCPSWithCont e2 k0
    -- TODO: seems code beyond should be safe but I should prove it!
    let t = runReader (extractTypeFromCExpr e1') Map.empty
    let x0' = CPS.Var x0 t
        x1' = CPS.Var x1 CPS.CTBottom
        x2' = CPS.Var x2 CPS.CTBottom
    kApplied <- k x0'
    let kif z = return $ CPS.CLetCont k0 x0' kApplied (CPS.CLetCont k1 x1' e1' $ CPS.CLetCont k2 x2' e2' (CPS.CIf z k1 k2))
    coreExprToCPS cond kif
coreExprToCPS (Core.Cons sName exprs) k = coreStructTranslation sName exprs k
coreExprToCPS (Core.FieldGet fName e) k = do
    (_, x) <- nextVar
    coreExprToCPS e $ \z@(CPS.Var _ (CPS.CTStruct s)) -> do
        fieldDefs <- asks (Map.! s)
        let t = trdTriple $ fromJust (find ((== fName) . fstTriple) fieldDefs)
            x' = CPS.Var x (coreTypeTranslation t)
        kApplied <- k x'
        return $ CPS.CLetFieldGet x' fName z kApplied
coreExprToCPS (Core.TupleCons exprs) k = coreTupleTranslation exprs k
coreExprToCPS (Core.TupleProj i e) k = do
    (_, x) <- nextVar
    coreExprToCPS e $ \z@(CPS.Var _ (CPS.CTTuple t _)) -> do
        let x' = CPS.Var x t
        kApplied <- k x'
        return $ CPS.CLetProj x' i z kApplied
coreExprToCPS (Core.Lit l) k = do
    (_, xName) <- nextVar
    let (val, t) = case l of
            Core.LFloat f -> (CPS.CLitFloat f, CPS.CTFloat)
            Core.LBool b  -> (CPS.CLitBool b, CPS.CTBool)
            Core.LInt i   -> (CPS.CLitInt i, CPS.CTInt)
    let x' = CPS.Var xName t
    kApplied <- k x'
    return $ CPS.CLetVal x' val kApplied
coreExprToCPS (Core.LetFun (Core.Prog (Core.VarId f t) args e1) e2) k = do
    (kName, _) <- nextVar
    e1' <- coreExprToCPSWithCont e1 kName
    e2' <- coreExprToCPS e2 k
    let t' = coreTypeTranslation (runIdentity t)
        f' = CPS.Var f t'
        cpsArgs = coreVarTranslation <$> args
    return $ CPS.CLetFun (CPS.CFunDef f' kName cpsArgs e1') e2'
coreExprToCPS (Core.UnOp op expr) k = do
    (_, resName) <- nextVar
    let opT t = case op of
            OpNeg -> t
            OpNot -> CPS.CTBool
        kprim x@(CPS.Var _ ctype) = do
            let resVar = CPS.Var resName (opT ctype)
            kApplied <- k resVar
            return $ CPS.CLetPrim resVar (CPS.CUnOp op) [x] kApplied
    coreExprToCPS expr kprim
coreExprToCPS (Core.BinOp op e1 e2) k = do
    (_, resName) <- nextVar
    let k2 z1 z2@(CPS.Var _ ctype) = do
            let resVar = CPS.Var resName (coreOpType op ctype)
            kApplied <- k resVar
            return $ CPS.CLetPrim resVar (CPS.CBinOp op) [z1, z2] kApplied
    let k1 z1 = coreExprToCPS e2 $ k2 z1
    coreExprToCPS e1 k1

coreExprToCPSWithCont :: Core.Expr Identity -> CPS.CVar -> TranslateM CPS.CExpr
coreExprToCPSWithCont (Core.Var (Core.VarId x t)) k = do
    let x' = CPS.Var x $ coreTypeTranslation (runIdentity t)
    return $ CPS.CAppCont k x'
coreExprToCPSWithCont e@Core.App {} k = do
    let (f, args) = aggregateApplications e
    coreExprToCPS f $ \fn -> coreExprRec fn args [] k
coreExprToCPSWithCont (Core.Let (Core.VarId n t) e1 e2) k = do
    (jName, _) <- nextVar
    e1' <- coreExprToCPSWithCont e1 jName
    e2' <- coreExprToCPSWithCont e2 k
    let n' = CPS.Var n $ coreTypeTranslation (runIdentity t)
    return $ CPS.CLetCont jName n' e2' e1'
coreExprToCPSWithCont (Core.If cond e1 e2) k = do
    (k1, x1) <- nextVar
    (k2, x2) <- nextVar
    e1' <- coreExprToCPSWithCont e1 k
    e2' <- coreExprToCPSWithCont e2 k
    let x1' = CPS.Var x1 CPS.CTBottom
        x2' = CPS.Var x2 CPS.CTBottom
    let kif z = return $ CPS.CLetCont k1 x1' e1' (CPS.CLetCont k2 x2' e2' (CPS.CIf z k1 k2))
    coreExprToCPS cond kif
coreExprToCPSWithCont (Core.Cons sName exprs) k =
    coreStructTranslation sName exprs (return . CPS.CAppCont k)
coreExprToCPSWithCont (Core.FieldGet fName e) k = do
    (_, x) <- nextVar
    coreExprToCPS e $ \z@(CPS.Var _ (CPS.CTStruct s)) -> do
        fieldDefs <- asks (Map.! s)
        let t = trdTriple $ fromJust (find ((== fName) . fstTriple) fieldDefs)
            x' = CPS.Var x (coreTypeTranslation t)
        return $ CPS.CLetFieldGet x' fName z (CPS.CAppCont k x')
coreExprToCPSWithCont (Core.TupleCons exprs) k = 
    coreTupleTranslation exprs (return . CPS.CAppCont k)
coreExprToCPSWithCont (Core.TupleProj i e) k = do
    (_, x) <- nextVar
    coreExprToCPS e $ \z@(CPS.Var _ (CPS.CTTuple t _)) -> do
        let x' = CPS.Var x t
        return $ CPS.CLetProj x' i z $ CPS.CAppCont k x'
coreExprToCPSWithCont (Core.Lit l) k = do
    (_, xName) <- nextVar
    let (val, t) = case l of
            Core.LFloat f -> (CPS.CLitFloat f, CPS.CTFloat)
            Core.LBool b  -> (CPS.CLitBool b, CPS.CTBool)
            Core.LInt i   -> (CPS.CLitInt i, CPS.CTInt)
    let x' = CPS.Var xName t
    return $ CPS.CLetVal x' val (CPS.CAppCont k x')
coreExprToCPSWithCont (Core.LetFun (Core.Prog (Core.VarId f t) args e1) e2) k = do
    (jName, _) <- nextVar
    e1' <- coreExprToCPSWithCont e1 jName
    e2' <- coreExprToCPSWithCont e2 k
    let t' = coreTypeTranslation (runIdentity t)
        f' = CPS.Var f t'
        cpsArgs = coreVarTranslation <$> args
    return $ CPS.CLetFun (CPS.CFunDef f' jName cpsArgs e1') e2'
coreExprToCPSWithCont (Core.UnOp op expr) k = do
    (_, resName) <- nextVar
    let opT t = case op of
            OpNeg -> t
            OpNot -> CPS.CTBool
        kprim x@(CPS.Var _ ctype) = do
            let resVar = CPS.Var resName (opT ctype)
            return $ CPS.CLetPrim resVar (CPS.CUnOp op) [x] (CPS.CAppCont k resVar)
    coreExprToCPS expr kprim
coreExprToCPSWithCont (Core.BinOp op e1 e2) k = do
    (_, resName) <- nextVar
    let k2 z1 z2@(CPS.Var _ ctype) = do
            let resVar = CPS.Var resName (coreOpType op ctype)
                kApplied = CPS.CAppCont k resVar
            return $ CPS.CLetPrim resVar (CPS.CBinOp op) [z1, z2] kApplied
        k1 z1 = coreExprToCPS e2 $ k2 z1
    coreExprToCPS e1 k1

coreExprRec :: CPS.Var -> [Core.Expr Identity] -> [CPS.Var] -> CPS.CVar -> TranslateM CPS.CExpr
coreExprRec fn (h:t) vars kName = coreExprToCPS h (\x -> coreExprRec fn t (x:vars) kName)
coreExprRec fn [] vars kName    = return $ CPS.CAppFun fn kName (reverse vars)

coreTupleTranslation :: [Core.Expr Identity] -> CCont -> TranslateM CPS.CExpr
coreTupleTranslation exprs kFun = 
    coreStructOrTupleTranslation exprs [] kFun typeCtr ctr
    where
        typeCtr = \vars -> CPS.CTTuple (CPS._varType $ head vars) (length vars)
        ctr = CPS.CTuple

coreStructTranslation :: String
                      -> [Core.Expr Identity]  
                      -> CCont 
                      -> TranslateM CPS.CExpr
coreStructTranslation structName exprs kFun =
    coreStructOrTupleTranslation exprs [] kFun typeCtr ctr
    where
        typeCtr = const (CPS.CTStruct structName)
        ctr = CPS.CStruct structName

coreStructOrTupleTranslation :: [Core.Expr Identity] 
                             -> [CPS.Var]
                             -> CCont
                             -> ([CPS.Var] -> CPS.CType)
                             -> ([CPS.Var] -> CPS.CVal)
                             -> TranslateM CPS.CExpr
coreStructOrTupleTranslation (h:t) vars kFun typeCtr ctr = 
    coreExprToCPS h $ 
        \x -> coreStructOrTupleTranslation t (x:vars) kFun typeCtr ctr
coreStructOrTupleTranslation [] vars kFun typeCtr ctr = do
    (_, x) <- nextVar
    let varT = typeCtr vars
        x' = CPS.Var x varT
        val = ctr (reverse vars)
    kApplied <- kFun x'
    return $ CPS.CLetVal x' val kApplied

coreVarTranslation :: Core.VarId Identity -> CPS.Var
coreVarTranslation (Core.VarId vn (Identity vt)) = 
    CPS.Var vn (coreTypeTranslation vt)

coreTypeTranslation :: Core.Type -> CPS.CType
coreTypeTranslation Core.TFloat = CPS.CTFloat
coreTypeTranslation Core.TBool = CPS.CTBool
coreTypeTranslation Core.TInt = CPS.CTInt
coreTypeTranslation (Core.TFun t1 t2) = 
    CPS.CTFun (coreTypeTranslation t1) (coreTypeTranslation t2)
coreTypeTranslation (Core.TTuple t i) = CPS.CTTuple (coreTypeTranslation t) i
coreTypeTranslation (Core.TStruct sName) = CPS.CTStruct sName
coreTypeTranslation (Core.TSampler i) = CPS.CTSampler i
coreTypeTranslation Core.TDummy = undefined
coreTypeTranslation (Core.TVar _) = undefined

coreOpType :: BinOp -> CPS.CType -> CPS.CType
coreOpType op t = case op of
    OpAdd   -> t
    OpMul   -> t
    OpSub   -> t
    OpDiv   -> t
    OpMod   -> t
    OpAnd   -> CPS.CTBool
    OpOr    -> CPS.CTBool
    OpEq    -> CPS.CTBool
    OpNotEq -> CPS.CTBool
    OpLT    -> CPS.CTBool
    OpLTEq  -> CPS.CTBool
    OpGT    -> CPS.CTBool
    OpGTEq  -> CPS.CTBool

extractTypeFromCExpr :: CPS.CExpr -> Reader (Map.Map String CPS.CType) CPS.CType
extractTypeFromCExpr (CPS.CLetVal (CPS.Var x t) _ e) = 
    local (Map.insert x t) (extractTypeFromCExpr e)
extractTypeFromCExpr (CPS.CLetProj (CPS.Var x t) _ _ e) =
    local (Map.insert x t) (extractTypeFromCExpr e)
extractTypeFromCExpr (CPS.CLetFieldGet (CPS.Var x t) _ _ e) =
    local (Map.insert x t) (extractTypeFromCExpr e)
extractTypeFromCExpr (CPS.CLetCont k (CPS.Var _ t) _e1 e2) = 
    local (Map.insert k t) (extractTypeFromCExpr e2)
extractTypeFromCExpr (CPS.CLetFun (CPS.CFunDef (CPS.Var f fT) _ _ _) e) =
    local (Map.insert f fT) (extractTypeFromCExpr e)
extractTypeFromCExpr (CPS.CAppCont _ (CPS.Var _ t)) = return t
extractTypeFromCExpr (CPS.CAppFun (CPS.Var f fT) _ _) = return $ CPS.resType fT
extractTypeFromCExpr (CPS.CLetPrim (CPS.Var x t) _ _ e) =
    local (Map.insert x t) (extractTypeFromCExpr e)
extractTypeFromCExpr (CPS.CIf _ x _) = asks (Map.! x)
