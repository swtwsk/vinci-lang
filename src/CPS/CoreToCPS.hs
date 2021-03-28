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

coreToCPS :: Core.Prog -> CPS.CProg
coreToCPS prog = evalVarSupply (runReaderT (coreToCPS' prog) Map.empty) supp
    where
        supp = [("k" ++ show x, "x" ++ show x) | x <- [(0 :: Int) ..]]

coreToCPS' :: Core.Prog -> TranslateM CPS.CProg
coreToCPS' (Core.Prog progName args expr) = do
    expr' <- coreExprToCPS expr k
    return $ CPS.CProcLam progName args expr'
    where k x = return $ CPS.CExit x

coreExprToCPS :: Core.Expr -> CCont -> TranslateM CPS.CExpr
coreExprToCPS (Core.Var x) k = k x
coreExprToCPS (Core.App e1 e2) k = do
    (kName, xName) <- nextVar
    kApplied <- k xName
    let k1 z1 = coreExprToCPS e2 $
            return . CPS.CLetCont kName xName kApplied . CPS.CAppFun z1 kName
    coreExprToCPS e1 k1
coreExprToCPS (Core.Lam n e) k = do
    (_, fName) <- nextVar
    (kName, _) <- nextVar
    kApplied <- k fName
    e' <- coreExprToCPS e (return . CPS.CAppCont kName)
    let letval = CPS.CLetVal fName (CPS.CLamCont kName n e') kApplied
    return letval
coreExprToCPS (Core.Let n e1 e2) k = do
    (jName, _) <- nextVar
    e1' <- coreExprToCPS e1 (return . CPS.CAppCont jName)
    e2' <- coreExprToCPS e2 k
    return $ CPS.CLetCont jName n e2' e1'
coreExprToCPS (Core.If cond e1 e2) k = do
    (k0, x0) <- nextVar
    (k1, x1) <- nextVar
    (k2, x2) <- nextVar
    kApplied <- k x0
    e1' <- coreExprToCPS e1 (return . CPS.CAppCont k0)
    e2' <- coreExprToCPS e2 (return . CPS.CAppCont k0)
    let kif z = return $ CPS.CLetCont k0 x0 kApplied (CPS.CLetCont k1 x1 e1' (CPS.CLetCont k2 x2 e2' (CPS.CIf z k1 k2)))
    coreExprToCPS cond kif
coreExprToCPS (Core.Lit (Core.LFloat f)) k = do
    (_, xName) <- nextVar
    kApplied <- k xName
    return $ CPS.CLetVal xName (CPS.CLitFloat f) kApplied
coreExprToCPS (Core.LetRec f x e1 e2) k = do
    (kName, _) <- nextVar
    e1' <- coreExprToCPS e1 (return . CPS.CAppCont kName)
    e2' <- coreExprToCPS e2 k
    return $ CPS.CLetFix f kName x e1' e2'
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
