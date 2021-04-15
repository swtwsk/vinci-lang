{-# OPTIONS_GHC -Wno-unused-matches #-}
module CPS.CoreToCPS (coreToCPS) where

import Control.Monad.Reader

import qualified Data.Map as Map
import Utils.VarSupply (VarSupply, evalVarSupply, nextVar)

import qualified Core.AST as Core
import Core.Utils (aggregateApplications)
import qualified CPS.AST as CPS

type RecDependentMap = Map.Map String Bool
type TranslateM = ReaderT RecDependentMap (VarSupply (String, String))

type CCont = CPS.Var -> TranslateM CPS.CExpr

coreToCPS :: Core.Prog -> CPS.CFunDef
coreToCPS prog = evalVarSupply (runReaderT (coreToCPS' prog) Map.empty) supp
    where
        supp = [("k" ++ show x, "x" ++ show x) | x <- [(0 :: Int) ..]]

coreToCPS' :: Core.Prog -> TranslateM CPS.CFunDef
coreToCPS' (Core.Prog _isRec progName args expr) = do
    (kName, _) <- nextVar
    expr' <- coreExprToCPSWithCont expr kName
    return $ CPS.CFunDef progName kName args expr'

coreExprToCPS :: Core.Expr -> CCont -> TranslateM CPS.CExpr
coreExprToCPS (Core.Var x) k = k x
coreExprToCPS e@Core.App {} k = do
    (kName, xName) <- nextVar
    kApplied <- k xName
    let (f, args) = aggregateApplications e
    coreExprToCPS f $ \fn -> 
        CPS.CLetCont kName xName kApplied <$> coreExprRec fn args [] kName
coreExprToCPS (Core.Let n e1 e2) k = do
    (jName, _) <- nextVar
    e1' <- coreExprToCPSWithCont e1 jName
    e2' <- coreExprToCPS e2 k
    return $ CPS.CLetCont jName n e2' e1'
coreExprToCPS (Core.If cond e1 e2) k = do
    (k0, x0) <- nextVar
    (k1, x1) <- nextVar
    (k2, x2) <- nextVar
    kApplied <- k x0
    e1' <- coreExprToCPSWithCont e1 k0
    e2' <- coreExprToCPSWithCont e2 k0
    let kif z = return $ CPS.CLetCont k0 x0 kApplied (CPS.CLetCont k1 x1 e1' (CPS.CLetCont k2 x2 e2' (CPS.CIf z k1 k2)))
    coreExprToCPS cond kif
coreExprToCPS (Core.TupleCons exprs) k = 
    coreTupleTranslation exprs [] k
coreExprToCPS (Core.TupleProj i e) k = do
    (_, x) <- nextVar
    kApplied <- k x
    coreExprToCPS e (\z -> return $ CPS.CLetProj x i z kApplied)
coreExprToCPS (Core.Lit l) k = do
    (_, xName) <- nextVar
    kApplied <- k xName
    let val = case l of
            Core.LFloat f -> CPS.CLitFloat f
            Core.LBool b  -> CPS.CLitBool b
    return $ CPS.CLetVal xName val kApplied
coreExprToCPS (Core.LetFun (Core.Prog Core.NonRec f args e1) e2) k = do
    (kName, _) <- nextVar
    e1' <- coreExprToCPSWithCont e1 kName
    e2' <- coreExprToCPS e2 k
    return $ CPS.CLetFun (CPS.CFunDef f kName args e1') e2'
coreExprToCPS (Core.LetFun (Core.Prog Core.Rec f args e1) e2) k = do
    (kName, _) <- nextVar
    e1' <- coreExprToCPSWithCont e1 kName
    e2' <- coreExprToCPS e2 k
    return $ CPS.CLetFix f kName args e1' e2'
coreExprToCPS (Core.UnOp op expr) k = do
    (_, resName) <- nextVar
    kApplied <- k resName
    let kprim x = return $ CPS.CLetPrim resName (CPS.CUnOp op) [x] kApplied
    coreExprToCPS expr kprim
coreExprToCPS (Core.BinOp op e1 e2) k = do
    (_, resName) <- nextVar
    kApplied <- k resName
    let k2 z1 z2 = return $ CPS.CLetPrim resName (CPS.CBinOp op) [z1, z2] kApplied
    let k1 z1 = coreExprToCPS e2 $ k2 z1
    coreExprToCPS e1 k1
-- coreExprToCPS _ _ = undefined

coreExprToCPSWithCont :: Core.Expr -> CPS.CVar -> TranslateM CPS.CExpr
coreExprToCPSWithCont (Core.Var x) k = return $ CPS.CAppCont k x
coreExprToCPSWithCont e@Core.App {} k = do
    let (f, args) = aggregateApplications e
    coreExprToCPS f $ \fn -> coreExprRec fn args [] k
coreExprToCPSWithCont (Core.Let n e1 e2) k = do
    (jName, _) <- nextVar
    e1' <- coreExprToCPSWithCont e1 jName
    e2' <- coreExprToCPSWithCont e2 k
    return $ CPS.CLetCont jName n e2' e1'
coreExprToCPSWithCont (Core.If cond e1 e2) k = do
    (k1, x1) <- nextVar
    (k2, x2) <- nextVar
    e1' <- coreExprToCPSWithCont e1 k
    e2' <- coreExprToCPSWithCont e2 k
    let kif z = return $ CPS.CLetCont k1 x1 e1' (CPS.CLetCont k2 x2 e2' (CPS.CIf z k1 k2))
    coreExprToCPS cond kif
coreExprToCPSWithCont (Core.TupleCons exprs) k = 
    coreTupleTranslation exprs [] (return . CPS.CAppCont k)
coreExprToCPSWithCont (Core.TupleProj i e) k = do
    (_, x) <- nextVar
    coreExprToCPS e (\z -> return $ CPS.CLetProj x i z $ CPS.CAppCont k x)
coreExprToCPSWithCont (Core.Lit l) k = do
    (_, xName) <- nextVar
    let val = case l of
            Core.LFloat f -> CPS.CLitFloat f
            Core.LBool b -> CPS.CLitBool b
    return $ CPS.CLetVal xName val (CPS.CAppCont k xName)
coreExprToCPSWithCont (Core.LetFun (Core.Prog Core.NonRec f args e1) e2) k = do
    (jName, _) <- nextVar
    e1' <- coreExprToCPSWithCont e1 jName
    e2' <- coreExprToCPSWithCont e2 k
    return $ CPS.CLetFun (CPS.CFunDef f jName args e1') e2'
coreExprToCPSWithCont (Core.LetFun (Core.Prog Core.Rec f args e1) e2) k = do
    (jName, _) <- nextVar
    e1' <- coreExprToCPSWithCont e1 jName
    e2' <- coreExprToCPSWithCont e2 k
    return $ CPS.CLetFix f jName args e1' e2'
coreExprToCPSWithCont (Core.UnOp op expr) k = do
    (_, resName) <- nextVar
    let kprim x = return $ 
            CPS.CLetPrim resName (CPS.CUnOp op) [x] (CPS.CAppCont k resName)
    coreExprToCPS expr kprim
coreExprToCPSWithCont (Core.BinOp op e1 e2) k = do
    (_, resName) <- nextVar
    let kApplied = CPS.CAppCont k resName
        k2 z1 z2 = return $ CPS.CLetPrim resName (CPS.CBinOp op) [z1, z2] kApplied
        k1 z1 = coreExprToCPS e2 $ k2 z1
    coreExprToCPS e1 k1

coreExprRec :: CPS.CVar -> [Core.Expr] -> [CPS.CVar] -> CPS.CVar -> TranslateM CPS.CExpr
coreExprRec fn (h:t) vars kName = coreExprToCPS h (\x -> coreExprRec fn t (x:vars) kName)
coreExprRec fn [] vars kName    = return $ CPS.CAppFun fn kName (reverse vars)

coreTupleTranslation :: [Core.Expr] -> [CPS.CVar] -> CCont -> TranslateM CPS.CExpr
coreTupleTranslation (h:t) vars kFun = 
    coreExprToCPS h (\x -> coreTupleTranslation t (x:vars) kFun)
coreTupleTranslation [] vars kFun = do
    (_, x) <- nextVar
    kApplied <- kFun x
    return $ CPS.CLetVal x (CPS.CTuple (reverse vars)) kApplied
