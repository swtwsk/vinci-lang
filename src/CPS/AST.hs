module CPS.AST where

import Control.Monad.Reader
import Control.Monad.Zip (mzip)
import Data.List (intercalate)
import qualified Data.Map as Map

import Core.Ops (BinOp, UnOp)
import Utils.VarSupply (VarSupply, evalVarSupply, nextVar)

type Var  = String
type CVar = String

data CExpr = CLetVal Var CVal CExpr             -- letval x = V in K
           | CLetProj Var Int Var CExpr         -- let x = #i y in K
           | CLetCont CVar Var CExpr CExpr      -- letcont k x = K in K'
           | CLetFun CFunDef CExpr              -- letfun F in K
           | CAppCont CVar Var                  -- k x
           | CAppFun Var CVar [Var]             -- f k xs
           | CLetPrim Var CPrimOp [Var] CExpr   -- letprim x = PrimOp [y] in K
           | CIf Var CVar CVar                  -- if x then k1 else k2
           | CLetFix Var CVar [Var] CExpr CExpr -- letfix f k x = K in K'
           deriving Eq

data CVal = CLitFloat Double
          | CLitBool Bool
          | CLamCont CVar Var CExpr  -- \k x -> K
          | CTuple [Var]
          deriving Eq

data CPrimOp = CBinOp BinOp 
             | CUnOp UnOp
             deriving Eq

data CFunDef = CFunDef Var CVar [Var] CExpr -- letproc proc k (args*) = E
           deriving Eq

-- ALPHA EQUALITIES
alphaEq :: (AlphaEq a) => a -> a -> Bool
x `alphaEq` y = evalVarSupply (runReaderT (x `alphaReq` y) (Map.empty, Map.empty)) supp
        where
            supp = ["#" ++ show i | i <- [(0 :: Int) ..]]

type ReaderMap = Map.Map String String
type AlphaEqT = ReaderT (ReaderMap, ReaderMap) (VarSupply String)

class (Eq a) => AlphaEq a where
    alphaReq :: a -> a -> AlphaEqT Bool

instance AlphaEq CExpr where
    (CLetVal x1 cval1 cexpr1) `alphaReq` (CLetVal x2 cval2 cexpr2) = do
        (varMap1, varMap2) <- ask
        newX <- nextVar
        let newMaps = (Map.insert x1 newX varMap1, Map.insert x2 newX varMap2)
        cvalEq <- cval1 `alphaReq` cval2
        cexpalphaEqCExpr <- local (const newMaps) (cexpr1 `alphaReq` cexpr2)
        return $ cvalEq && cexpalphaEqCExpr
    (CLetProj x1 i1 t1 c1) `alphaReq` (CLetProj x2 i2 t2 c2) = do
        (varMap1, varMap2) <- ask
        newX <- nextVar
        let cMaps = (Map.insert x1 newX varMap1, Map.insert x2 newX varMap2)
        cEq <- local (const cMaps) $ c1 `alphaReq` c2
        let iEq = i1 == i2
            tEq = lookupEquality t1 t2 cMaps
        return $ cEq && iEq && tEq
    (CLetCont k1 x1 c11 c21) `alphaReq` (CLetCont k2 x2 c12 c22) = do
        (varMap1, varMap2) <- ask
        newX <- nextVar
        newK <- nextVar
        let c1Maps = (Map.insert x1 newX varMap1, Map.insert x2 newX varMap2)
            c2Maps = (Map.insert k1 newK varMap1, Map.insert k2 newK varMap2)
        c1Eq <- local (const c1Maps) $ c11 `alphaReq` c12
        c2Eq <- local (const c2Maps) $ c21 `alphaReq` c22
        return $ c1Eq && c2Eq
    (CLetFun (CFunDef f1 _ _ _) c1) `alphaReq` (CLetFun (CFunDef f2 _ _ _) c2) = do
        (varMap1, varMap2) <- ask
        newF <- nextVar
        let newMap = (Map.insert f1 newF varMap1, Map.insert f2 newF varMap2)
        local (const newMap) $ c1 `alphaReq` c2
    (CAppCont k1 x1) `alphaReq` (CAppCont k2 x2) = do
        kEq <- asks $ lookupEquality k1 k2
        xEq <- asks $ lookupEquality x1 x2
        return $ xEq && kEq
    (CAppFun f1 k1 x1) `alphaReq` (CAppFun f2 k2 x2) = do
        maps <- ask
        let fEq    = lookupEquality f1 f2 maps
            kEq    = lookupEquality k1 k2 maps
            xLenEq = length x1 == length x2
            xEq    = and $ (\(x, y) -> lookupEquality x y maps) <$> zip x1 x2
        return $ fEq && xLenEq && xEq && kEq
    (CLetPrim x1 primOp1 args1 c1) `alphaReq` (CLetPrim x2 primOp2 args2 c2) = do
        maps <- ask
        newX <- nextVar
        let (varMap1, varMap2) = maps
            argsEq = all (\(a, b) -> lookupEquality a b maps) $ zip args1 args2
            newMaps = (Map.insert x1 newX varMap1, Map.insert x2 newX varMap2)
        cEq <- local (const newMaps) $ c1 `alphaReq` c2
        return $ primOp1 == primOp2 && argsEq && cEq
    (CIf x1 k11 k21) `alphaReq` (CIf x2 k12 k22) = do
        maps <- ask
        let xEq  = lookupEquality x1 x2 maps
            k1Eq = lookupEquality k11 k12 maps
            k2Eq = lookupEquality k21 k22 maps
        return $ xEq && k1Eq && k2Eq
    (CLetFix f1 k1 x1 c11 c21) `alphaReq` (CLetFix f2 k2 x2 c12 c22) = do
        (varMap1, varMap2) <- ask
        (newF, newK) <- mzip nextVar nextVar
        newXs <- replicateM (length x1) nextVar
        let xLenEq   = length x1 == length x2
            newMap1  = Map.insert f1 newF $ Map.insert k1 newK $ 
                Map.union varMap1 (Map.fromList $ zip x1 newXs)
            newMap2  = Map.insert f2 newF $ Map.insert k2 newK $
                Map.union varMap2 (Map.fromList $ zip x2 newXs)
            newMapc2 = (Map.insert f1 newF varMap1, Map.insert f2 newF varMap2)
        c1Eq <- local (const (newMap1, newMap2)) $ c11 `alphaReq` c12
        c2Eq <- local (const newMapc2) $ c21 `alphaReq` c22
        return $ xLenEq && c1Eq && c2Eq
    _ `alphaReq` _ = return False

instance AlphaEq CVal where
    (CLitFloat f1) `alphaReq` (CLitFloat f2) = return $ f1 == f2
    (CLitBool b1) `alphaReq` (CLitBool b2) = return $ b1 == b2
    (CLamCont k1 x1 c1) `alphaReq` (CLamCont k2 x2 c2) = do
        (varMap1, varMap2) <- ask
        (newK, newX) <- mzip nextVar nextVar
        let newMap1 = Map.insert k1 newK $ Map.insert x1 newX varMap1
            newMap2 = Map.insert k2 newK $ Map.insert x2 newX varMap2
        local (const (newMap1, newMap2)) $ c1 `alphaReq` c2
    (CTuple xs1) `alphaReq` (CTuple xs2) = do
        maps <- ask
        return $ all (\(x, y) -> lookupEquality x y maps) $ zip xs1 xs2
    _ `alphaReq` _ = return False

instance AlphaEq CFunDef where
    (CFunDef f1 k1 args1 c1) `alphaReq` (CFunDef f2 k2 args2 c2) = do
        (varMap1, varMap2) <- ask
        (newF, newK) <- mzip nextVar nextVar
        let argsLen = length args1
            argsLenEq = length args1 == length args2
        newArgs <- replicateM argsLen nextVar
        let newMap1 = Map.insert f1 newF $ Map.insert k1 newK $ 
                Map.union (Map.fromList $ zip args1 newArgs) varMap1
            newMap2 = Map.insert f2 newF $ Map.insert k2 newK $ 
                Map.union (Map.fromList $ zip args2 newArgs) varMap2
        cEq <- local (const (newMap1, newMap2)) $ c1 `alphaReq` c2
        return $ argsLenEq && cEq

lookupEquality :: String -> String -> (ReaderMap, ReaderMap) -> Bool
lookupEquality x1 x2 (varMap1, varMap2) = case (lookup1, lookup2) of
    (Just x1', Just x2') -> x1' == x2'
    _ -> False
    where
        lookup1 = Map.lookup x1 varMap1
        lookup2 = Map.lookup x2 varMap2

-- SHOWS
instance Show CExpr where
    show (CLetVal x cval cexpr) = "letval " ++ x ++ " = (" ++ show cval ++ ") in " ++ show cexpr
    show (CLetProj x i t c) = "let " ++ x ++ " = π" ++ show i ++ " " ++ t ++ " in " ++ show c
    show (CLetCont k x c1 c2) = "letcont " ++ k ++ " " ++ x ++ " = (" ++ show c1 ++ ") in " ++ show c2
    show (CLetFun f cexpr) = "letfun " ++ show f ++ " in " ++ show cexpr
    show (CAppCont k x) = k ++ " " ++ x
    show (CAppFun f k args) = f ++ " " ++ k ++ " " ++ unwords args
    show (CLetPrim x primOp args cexpr) = "letprim " ++ x ++ " = (" ++ show primOp ++ " " ++ unwords args ++ ") in " ++ show cexpr
    show (CIf x k1 k2) = "if " ++ x ++ " then " ++ k1 ++ " else " ++ k2
    show (CLetFix f k args c1 c2) = "letfix " ++ f ++ " " ++ k ++ " " ++ unwords args ++ " = (" ++ show c1 ++ ") in " ++ show c2

instance Show CVal where
    show (CLitFloat f) = show f
    show (CLitBool b) = show b
    show (CLamCont k x cexpr) = "λ " ++ k ++ " " ++ x ++ " -> " ++ show cexpr
    show (CTuple vars) = "(" ++ intercalate ", " vars ++ ")"

instance Show CPrimOp where
    show (CBinOp op) = show op
    show (CUnOp op)  = show op

instance Show CFunDef where
    show (CFunDef procName k args e) = 
        procName ++ " " ++ k ++ " " ++ unwords args ++ " = " ++ show e
