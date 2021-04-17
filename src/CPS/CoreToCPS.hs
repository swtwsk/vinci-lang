{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE TupleSections #-}
module CPS.CoreToCPS (coreToCPS) where

import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as Map
import Utils.VarSupply (VarSupply, evalVarSupply, nextVar)

import qualified Core.AST as Core
import Core.Ops
import Core.Utils (aggregateApplications)
import qualified CPS.AST as CPS

type TranslateM = VarSupply (String, String)
type CCont = CPS.Var -> TranslateM CPS.CExpr

coreToCPS :: Core.Prog Identity -> CPS.CFunDef
coreToCPS prog = evalVarSupply (coreToCPS' prog) supp
    where
        supp = [("_k" ++ show x, "_x" ++ show x) | x <- [(0 :: Int) ..]]

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
coreExprToCPS (Core.TupleCons exprs) k = 
    coreTupleTranslation exprs [] k
coreExprToCPS (Core.TupleProj i e) k = do
    (_, x) <- nextVar
    coreExprToCPS e $ \z@(CPS.Var _ t) -> do
        let x' = CPS.Var x t
        kApplied <- k x'
        return $ CPS.CLetProj x' i z kApplied
coreExprToCPS (Core.Lit l) k = do
    (_, xName) <- nextVar
    let (val, t) = case l of
            Core.LFloat f -> (CPS.CLitFloat f, CPS.CTFloat)
            Core.LBool b  -> (CPS.CLitBool b, CPS.CTBool)
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
    let opT = case op of
            OpNeg -> CPS.CTFloat
            OpNot -> CPS.CTBool
    let resVar = CPS.Var resName opT
    kApplied <- k resVar
    let kprim x = return $ CPS.CLetPrim resVar (CPS.CUnOp op) [x] kApplied
    coreExprToCPS expr kprim
coreExprToCPS (Core.BinOp op e1 e2) k = do
    (_, resName) <- nextVar
    let opT = case op of
            OpAdd -> CPS.CTFloat
            OpMul -> CPS.CTFloat
            OpSub -> CPS.CTFloat
            OpDiv -> CPS.CTFloat
            OpMod -> CPS.CTFloat
            OpAnd -> CPS.CTBool
            OpOr  -> CPS.CTBool
            OpLT  -> CPS.CTBool
            OpEq  -> CPS.CTBool
    let resVar = CPS.Var resName opT
    kApplied <- k resVar
    let k2 z1 z2 = return $ CPS.CLetPrim resVar (CPS.CBinOp op) [z1, z2] kApplied
    let k1 z1 = coreExprToCPS e2 $ k2 z1
    coreExprToCPS e1 k1
-- coreExprToCPS _ _ = undefined

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
coreExprToCPSWithCont (Core.TupleCons exprs) k = 
    coreTupleTranslation exprs [] (return . CPS.CAppCont k)
coreExprToCPSWithCont (Core.TupleProj i e) k = do
    (_, x) <- nextVar
    coreExprToCPS e $ \z@(CPS.Var _ t) -> do
        let x' = CPS.Var x t
        return $ CPS.CLetProj x' i z $ CPS.CAppCont k x'
coreExprToCPSWithCont (Core.Lit l) k = do
    (_, xName) <- nextVar
    let (val, t) = case l of
            Core.LFloat f -> (CPS.CLitFloat f, CPS.CTFloat)
            Core.LBool b  -> (CPS.CLitBool b, CPS.CTBool)
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
    let opT = case op of
            OpNeg -> CPS.CTFloat
            OpNot -> CPS.CTBool
        resVar = CPS.Var resName opT
        kprim x = return $ 
            CPS.CLetPrim resVar (CPS.CUnOp op) [x] (CPS.CAppCont k resVar)
    coreExprToCPS expr kprim
coreExprToCPSWithCont (Core.BinOp op e1 e2) k = do
    (_, resName) <- nextVar
    let opT = case op of
            OpAdd -> CPS.CTFloat
            OpMul -> CPS.CTFloat
            OpSub -> CPS.CTFloat
            OpDiv -> CPS.CTFloat
            OpMod -> CPS.CTFloat
            OpAnd -> CPS.CTBool
            OpOr  -> CPS.CTBool
            OpLT  -> CPS.CTBool
            OpEq  -> CPS.CTBool
        resVar = CPS.Var resName opT
        kApplied = CPS.CAppCont k resVar
        k2 z1 z2 = return $ CPS.CLetPrim resVar (CPS.CBinOp op) [z1, z2] kApplied
        k1 z1 = coreExprToCPS e2 $ k2 z1
    coreExprToCPS e1 k1

coreExprRec :: CPS.Var -> [Core.Expr Identity] -> [CPS.Var] -> CPS.CVar -> TranslateM CPS.CExpr
coreExprRec fn (h:t) vars kName = coreExprToCPS h (\x -> coreExprRec fn t (x:vars) kName)
coreExprRec fn [] vars kName    = return $ CPS.CAppFun fn kName (reverse vars)

coreTupleTranslation :: [Core.Expr Identity] -> [CPS.Var] -> CCont -> TranslateM CPS.CExpr
coreTupleTranslation (h:t) vars kFun = 
    coreExprToCPS h (\x -> coreTupleTranslation t (x:vars) kFun)
coreTupleTranslation [] vars kFun = do
    let varT = CPS.CTTuple (CPS._varType $ head vars) (length vars)
    (_, x) <- nextVar
    let x' = CPS.Var x varT
    kApplied <- kFun x'
    return $ CPS.CLetVal x' (CPS.CTuple (reverse vars)) kApplied

coreVarTranslation :: Core.VarId Identity -> CPS.Var
coreVarTranslation (Core.VarId vn (Identity vt)) = 
    CPS.Var vn (coreTypeTranslation vt)

coreTypeTranslation :: Core.Type -> CPS.CType
coreTypeTranslation Core.TFloat = CPS.CTFloat
coreTypeTranslation Core.TBool = CPS.CTBool
coreTypeTranslation (Core.TFun t1 t2) = 
    CPS.CTFun (coreTypeTranslation t1) (coreTypeTranslation t2)
coreTypeTranslation (Core.TTuple t i) = CPS.CTTuple (coreTypeTranslation t) i
coreTypeTranslation Core.TDummy = undefined

extractTypeFromCExpr :: CPS.CExpr -> Reader (Map.Map String CPS.CType) CPS.CType
extractTypeFromCExpr (CPS.CLetVal (CPS.Var x t) _ e) = 
    local (Map.insert x t) (extractTypeFromCExpr e)
extractTypeFromCExpr (CPS.CLetProj (CPS.Var x t) _ _ e) =
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
