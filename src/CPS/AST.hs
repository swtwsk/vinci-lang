module CPS.AST where

import Control.Monad.Reader
import Control.Monad.Zip (mzip)
import Data.List (intercalate)
import qualified Data.Map as Map

import Core.Ops (BinOp, UnOp)
import Utils.VarSupply (VarSupply, evalVarSupply, nextVar)

data Var  = Var { _varName :: String, _varType :: CType }
          deriving Eq
type CVar = String

data CExpr = CLetVal Var CVal CExpr             -- letval x = V in K
           | CLetProj Var Int Var CExpr         -- let x = #i y in K
           | CLetFieldGet Var String Var CExpr  -- let x = #f y in K
           | CLetCont CVar Var CExpr CExpr      -- letcont k x = K in K'
           | CLetFun CFunDef CExpr              -- letfun F in K
           | CAppCont CVar Var                  -- k x
           | CAppFun Var CVar [Var]             -- f k xs
           | CLetPrim Var CPrimOp [Var] CExpr   -- letprim x = PrimOp [y] in K
           | CIf Var CVar CVar                  -- if x then k1 else k2
           deriving Eq

data CVal = CLitFloat Double
          | CLitBool Bool
          | CLitInt Int
          | CTuple [Var]
          | CStruct String [Var]
          deriving Eq

data CPrimOp = CBinOp BinOp 
             | CUnOp UnOp
             deriving Eq

data CFunDef = CFunDef Var CVar [Var] CExpr -- letproc proc k (args*) = E
           deriving Eq

data CType = CTFloat
           | CTBool
           | CTInt
           | CTFun CType CType
           | CTTuple CType Int
           | CTStruct String
           | CTBottom            -- meaning applied continuation
           deriving Eq

resType :: CType -> CType
resType (CTFun _ t2) = resType t2
resType t = t

-- ALPHA EQUALITIES
alphaEq :: (AlphaEq a) => a -> a -> Bool
x `alphaEq` y = evalVarSupply (runReaderT (x `alphaReq` y) (Map.empty, Map.empty)) supp
        where
            supp = ["#" ++ show i | i <- [(0 :: Int) ..]]

type ReaderMap = Map.Map String String
type AlphaEqT = ReaderT (ReaderMap, ReaderMap) (VarSupply String)

class (Eq a) => AlphaEq a where
    alphaReq :: a -> a -> AlphaEqT Bool

instance AlphaEq Var where
    (Var n1 t1) `alphaReq` (Var n2 t2) = do
        varMaps <- ask
        let tEq = t1 == t2
        let nEq = lookupEquality n1 n2 varMaps
        return $ tEq && nEq

instance AlphaEq CExpr where
    (CLetVal v1 cval1 cexpr1) `alphaReq` (CLetVal v2 cval2 cexpr2) = do
        (varMap1, varMap2) <- ask
        newX <- nextVar
        let (x1, x2) = (_varName v1, _varName v2)
        let newMaps = (Map.insert x1 newX varMap1, Map.insert x2 newX varMap2)
        cvalEq <- cval1 `alphaReq` cval2
        vEq <- local (const newMaps) (v1 `alphaReq` v2)
        cexpalphaEqCExpr <- local (const newMaps) (cexpr1 `alphaReq` cexpr2)
        return $ cvalEq && vEq && cexpalphaEqCExpr
    (CLetProj v1 i1 t1 c1) `alphaReq` (CLetProj v2 i2 t2 c2) = do
        (varMap1, varMap2) <- ask
        newX <- nextVar
        let (x1, x2) = (_varName v1, _varName v2)
        let cMaps = (Map.insert x1 newX varMap1, Map.insert x2 newX varMap2)
        cEq <- local (const cMaps) $ c1 `alphaReq` c2
        let iEq = i1 == i2
        tEq <- local (const cMaps) (t1 `alphaReq` t2)
        return $ cEq && iEq && tEq
    (CLetCont k1 v1 c11 c21) `alphaReq` (CLetCont k2 v2 c12 c22) = do
        (varMap1, varMap2) <- ask
        newX <- nextVar
        newK <- nextVar
        let (x1, x2) = (_varName v1, _varName v2)
        let c1Maps = (Map.insert x1 newX varMap1, Map.insert x2 newX varMap2)
            c2Maps = (Map.insert k1 newK varMap1, Map.insert k2 newK varMap2)
        c1Eq <- local (const c1Maps) $ c11 `alphaReq` c12
        vEq  <- local (const c1Maps) $ v1 `alphaReq` v2
        c2Eq <- local (const c2Maps) $ c21 `alphaReq` c22
        return $ c1Eq && vEq && c2Eq
    (CLetFun (CFunDef fv1 k1 v1 c11) c21) `alphaReq` (CLetFun (CFunDef fv2 k2 v2 c12) c22) = do
        (varMap1, varMap2) <- ask
        (newF, newK) <- mzip nextVar nextVar
        let (x1, x2) = (_varName <$> v1, _varName <$> v2)
            (f1, f2) = (_varName fv1, _varName fv2)
        newXs <- replicateM (length x1) nextVar
        let xLenEq   = length x1 == length x2
            newMap1  = Map.insert f1 newF $ Map.insert k1 newK $ 
                Map.union varMap1 (Map.fromList $ zip x1 newXs)
            newMap2  = Map.insert f2 newF $ Map.insert k2 newK $
                Map.union varMap2 (Map.fromList $ zip x2 newXs)
            newMapc2 = (Map.insert f1 newF varMap1, Map.insert f2 newF varMap2)
        vEq  <- local (const (newMap1, newMap2)) $ zipWithM alphaReq v1 v2
        c1Eq <- local (const (newMap1, newMap2)) $ c11 `alphaReq` c12
        fEq  <- local (const newMapc2) $ fv1 `alphaReq` fv2
        c2Eq <- local (const newMapc2) $ c21 `alphaReq` c22
        return $ xLenEq && and vEq && c1Eq && fEq && c2Eq
    (CAppCont k1 x1) `alphaReq` (CAppCont k2 x2) = do
        kEq <- asks $ lookupEquality k1 k2
        xEq <- x1 `alphaReq` x2
        return $ xEq && kEq
    (CAppFun f1 k1 v1) `alphaReq` (CAppFun f2 k2 v2) = do
        maps <- ask
        fEq <- f1 `alphaReq` f2
        vEq <- and <$> zipWithM alphaReq v1 v2
        let kEq    = lookupEquality k1 k2 maps
            xLenEq = length v1 == length v2
        return $ fEq && xLenEq && vEq && kEq
    (CLetPrim v1 primOp1 args1 c1) `alphaReq` (CLetPrim v2 primOp2 args2 c2) = do
        maps <- ask
        newX <- nextVar
        let (x1, x2) = (_varName v1, _varName v2)
            (varMap1, varMap2) = maps
            newMaps = (Map.insert x1 newX varMap1, Map.insert x2 newX varMap2)
        argsEq <- and <$> zipWithM alphaReq args1 args2
        vEq <- local (const newMaps) $ v1 `alphaReq` v2
        cEq <- local (const newMaps) $ c1 `alphaReq` c2
        return $ primOp1 == primOp2 && argsEq && cEq && vEq
    (CIf x1 k11 k21) `alphaReq` (CIf x2 k12 k22) = do
        maps <- ask
        xEq <- x1 `alphaReq` x2
        let k1Eq = lookupEquality k11 k12 maps
            k2Eq = lookupEquality k21 k22 maps
        return $ xEq && k1Eq && k2Eq
    _ `alphaReq` _ = return False

instance AlphaEq CVal where
    (CLitFloat f1) `alphaReq` (CLitFloat f2) = return $ f1 == f2
    (CLitBool b1) `alphaReq` (CLitBool b2) = return $ b1 == b2
    (CTuple xs1) `alphaReq` (CTuple xs2) = and <$> zipWithM alphaReq xs1 xs2
    _ `alphaReq` _ = return False

instance AlphaEq CFunDef where
    (CFunDef fv1 k1 v1 c1) `alphaReq` (CFunDef fv2 k2 v2 c2) = do
        (varMap1, varMap2) <- ask
        (newF, newK) <- mzip nextVar nextVar
        let (x1, x2) = (_varName <$> v1, _varName <$> v2)
            (f1, f2) = (_varName fv1, _varName fv2)
        let argsLen = length v1
            argsLenEq = length v1 == length v2
        newArgs <- replicateM argsLen nextVar
        let newMap1 = Map.insert f1 newF $ Map.insert k1 newK $ 
                Map.union (Map.fromList $ zip x1 newArgs) varMap1
            newMap2 = Map.insert f2 newF $ Map.insert k2 newK $ 
                Map.union (Map.fromList $ zip x2 newArgs) varMap2
        fEq  <- local (const (newMap1, newMap2)) $ fv1 `alphaReq` fv2
        vEq  <- local (const (newMap1, newMap2)) $ zipWithM alphaReq v1 v2
        cEq  <- local (const (newMap1, newMap2)) $ c1 `alphaReq` c2
        return $ argsLenEq && fEq && and vEq && cEq

lookupEquality :: String -> String -> (ReaderMap, ReaderMap) -> Bool
lookupEquality x1 x2 (varMap1, varMap2) = case (lookup1, lookup2) of
    (Just x1', Just x2') -> x1' == x2'
    _ -> False
    where
        lookup1 = Map.lookup x1 varMap1
        lookup2 = Map.lookup x2 varMap2

-- SHOWS
instance Show Var where
    show (Var n t) = "(" ++ n ++ " : " ++ show t ++ ")"

instance Show CExpr where
    show (CLetVal x cval cexpr) = "letval " ++ show x ++ " = (" ++ show cval ++ ") in " ++ show cexpr
    show (CLetProj x i t c) = "let " ++ show x ++ " = π" ++ show i ++ " " ++ show t ++ " in " ++ show c
    show (CLetFieldGet x f struct c) = "let " ++ show x ++ " = " ++ show struct ++ "." ++ f ++ " in " ++ show c
    show (CLetCont k x c1 c2) = "letcont " ++ k ++ " " ++ show x ++ " = (" ++ show c1 ++ ") in " ++ show c2
    show (CLetFun f cexpr) = "letfun " ++ show f ++ " in " ++ show cexpr
    show (CAppCont k x) = k ++ " " ++ show x
    show (CAppFun f k args) = show f ++ " " ++ k ++ " " ++ unwords (show <$> args)
    show (CLetPrim x primOp args cexpr) = "letprim " ++ show x ++ " = (" ++ show primOp ++ " " ++ unwords (show <$> args) ++ ") in " ++ show cexpr
    show (CIf x k1 k2) = "if " ++ show x ++ " then " ++ k1 ++ " else " ++ k2

instance Show CVal where
    show (CLitFloat f) = show f
    show (CLitBool b) = show b
    show (CLitInt i) = show i
    show (CTuple vars) = "(" ++ intercalate ", " (show <$> vars) ++ ")"
    show (CStruct sName vars) = 
        sName ++ " { " ++ intercalate ", " (show <$> vars) ++ " }"

instance Show CPrimOp where
    show (CBinOp op) = show op
    show (CUnOp op)  = show op

instance Show CFunDef where
    show (CFunDef procName k args e) = 
        show procName ++ " " ++ k ++ " " ++ unwords (show <$> args) ++ " = " ++ show e

instance Show CType where
    show t = case t of
        CTFloat -> "Float"
        CTBool -> "Bool"
        CTInt -> "Int"
        CTFun t1@CTFun{} t2@CTFun{} -> 
            "(" ++ show t1 ++ ") -> (" ++ show t2 ++ ")"
        CTFun t1 t2@CTFun{} -> show t1 ++ " -> (" ++ show t2 ++ ")"
        CTFun t1@CTFun{} t2 -> "(" ++ show t1 ++ ") -> " ++ show t2
        CTFun t1 t2 -> show t1 ++ " -> " ++ show t2
        CTTuple t' i -> intercalate " × " (show <$> replicate i t')
        CTStruct sName -> sName
        CTBottom -> "⊥"
