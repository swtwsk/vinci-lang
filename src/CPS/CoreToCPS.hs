{-# OPTIONS_GHC -Wno-unused-matches #-}
module CPS.CoreToCPS (coreToCPS) where

import Control.Monad.Reader

import qualified Data.Map as Map
import Utils.VarSupply (VarSupply, evalVarSupply, nextVar)

import qualified Core.AST as Core
import qualified CPS.AST as CPS

type RecDependentMap = Map.Map String Bool
type TranslateM = ReaderT RecDependentMap (VarSupply (String, String))

type CCont = String -> TranslateM CPS.CExpr
type CVar  = String

coreToCPS :: Core.Prog -> CPS.CProg
coreToCPS prog = evalVarSupply (runReaderT (coreToCPS' prog) Map.empty) supp
    where
        supp = [("k" ++ show x, "x" ++ show x) | x <- [(0 :: Int) ..]]

coreToCPS' :: Core.Prog -> TranslateM CPS.CProg
coreToCPS' (Core.Prog progName args expr) = do
    (kName, _) <- nextVar
    expr' <- coreExprToCPSWithCont expr kName
    return $ CPS.CProcLam progName kName args expr'

coreExprToCPS :: Core.Expr -> CCont -> TranslateM CPS.CExpr
coreExprToCPS (Core.Var x) k = k x
coreExprToCPS e@Core.App {} k = do
    (kName, xName) <- nextVar
    kApplied <- k xName
    let (f, args) = aggregateApplications e
    coreExprToCPS f $ \fn -> 
        CPS.CLetCont kName xName kApplied <$> coreExprRec fn args [] kName
    -- coreExprToCPS f $ \fn -> coreExprRec fn args [] (kName, xName) kApplied
    -- where
    --     coreExprRec :: CVar -> [Core.Expr] -> [CVar] -> (CVar, CVar) -> CPS.CExpr -> TranslateM CPS.CExpr
    --     coreExprRec fn (h:t) vars kXs kApplied         = coreExprToCPS h (\x -> coreExprRec fn t (x:vars) kXs kApplied)
    --     coreExprRec fn [] vars (kName, xName) kApplied = return $ CPS.CLetCont kName xName kApplied $ CPS.CAppFun fn kName (reverse vars)
coreExprToCPS (Core.Lam n e) k = do
    (kName, fName) <- nextVar
    kApplied <- k fName
    e' <- coreExprToCPSWithCont e kName
    return $ CPS.CLetVal fName (CPS.CLamCont kName n e') kApplied
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
coreExprToCPS (Core.Lit (Core.LFloat f)) k = do
    (_, xName) <- nextVar
    kApplied <- k xName
    return $ CPS.CLetVal xName (CPS.CLitFloat f) kApplied
coreExprToCPS (Core.LetRec f args e1 e2) k = do
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
coreExprToCPS _ _ = undefined

coreExprToCPSWithCont :: Core.Expr -> CVar -> TranslateM CPS.CExpr
coreExprToCPSWithCont (Core.Var x) k = return $ CPS.CAppCont k x
coreExprToCPSWithCont e@Core.App {} k = do
    let (f, args) = aggregateApplications e
    coreExprToCPS f $ \fn -> coreExprRec fn args [] k
coreExprToCPSWithCont (Core.Lam n e) k = do
    (kName, fName) <- nextVar
    e' <- coreExprToCPSWithCont e kName
    return $ CPS.CLetVal fName (CPS.CLamCont kName n e') (CPS.CAppCont k fName)
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
coreExprToCPSWithCont (Core.Lit (Core.LFloat f)) k = do
    (_, xName) <- nextVar
    return $ CPS.CLetVal xName (CPS.CLitFloat f) (CPS.CAppCont k xName)
coreExprToCPSWithCont (Core.LetRec f args e1 e2) k = do
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
coreExprToCPSWithCont _ _ = undefined

-- (...((f x1) x2) ... xn) = (f, [x1, ..., xn])
-- (f x1) (g x2) = (f x1, [g x2])
aggregateApplications :: Core.Expr -> (Core.Expr, [Core.Expr])
aggregateApplications (Core.App e1@Core.App {} e2) = 
    let (fn, args) = aggregateApplications e1 in
    (fn, args ++ [e2])
aggregateApplications (Core.App e1 e2) = (e1, [e2])
aggregateApplications e = (e, [])

coreExprRec :: CVar -> [Core.Expr] -> [CVar] -> CVar -> TranslateM CPS.CExpr
coreExprRec fn (h:t) vars kName = coreExprToCPS h (\x -> coreExprRec fn t (x:vars) kName)
coreExprRec fn [] vars kName    = return $ CPS.CAppFun fn kName (reverse vars)
