module CPS.AST where

import Control.Monad.Reader
import Control.Monad.Zip (mzip)
import qualified Data.Map as Map

import Core.AST (UnOp, BinOp)
import Utils.VarSupply (VarSupply, evalVarSupply, nextVar)

data CExpr = CLetVal String CVal CExpr                  -- letval x = V in K
           | CLetCont String String CExpr CExpr         -- letcont k x = K in K'
           | CAppCont String String                     -- k x
           | CAppFun String String [String]             -- f k xs
           | CLetPrim String CPrimOp [String] CExpr     -- letprim x = PrimOp [y] in K
           | CIf String String String                   -- if x then k1 else k2
           | CLetFix String String [String] CExpr CExpr -- letfix f k x = K in K'
           | CExit String                               -- end continuation
           deriving Eq

data CVal = CLitFloat Double
          | CLamCont String String CExpr  -- \k x -> K
          deriving Eq

data CPrimOp = CBinOp BinOp 
             | CUnOp UnOp
             deriving Eq

data CProg = CProcLam String String [String] CExpr -- letproc proc k (args*) = E
--            | CJumpLam [String] CExpr
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
    (CLetCont k1 x1 c11 c21) `alphaReq` (CLetCont k2 x2 c12 c22) = do
        (varMap1, varMap2) <- ask
        newX <- nextVar
        newK <- nextVar
        let c1Maps = (Map.insert x1 newX varMap1, Map.insert x2 newX varMap2)
            c2Maps = (Map.insert k1 newK varMap1, Map.insert k2 newK varMap2)
        c1Eq <- local (const c1Maps) $ c11 `alphaReq` c12
        c2Eq <- local (const c2Maps) $ c21 `alphaReq` c22
        return $ c1Eq && c2Eq
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
    (CExit x1) `alphaReq` (CExit x2) = asks $ lookupEquality x1 x2
    _ `alphaReq` _ = return False

instance AlphaEq CVal where
    (CLitFloat f1) `alphaReq` (CLitFloat f2) = return $ f1 == f2
    (CLamCont k1 x1 c1) `alphaReq` (CLamCont k2 x2 c2) = do
        (varMap1, varMap2) <- ask
        (newK, newX) <- mzip nextVar nextVar
        let newMap1 = Map.insert k1 newK $ Map.insert x1 newX varMap1
            newMap2 = Map.insert k2 newK $ Map.insert x2 newX varMap2
        local (const (newMap1, newMap2)) $ c1 `alphaReq` c2
    _ `alphaReq` _ = return False

instance AlphaEq CProg where
    (CProcLam _ k1 args1 c1) `alphaReq` (CProcLam _ k2 args2 c2) = do
        (varMap1, varMap2) <- ask
        newK <- nextVar
        let argsLen = length args1
            argsLenEq = length args1 == length args2
        newArgs <- replicateM argsLen nextVar
        let mapFold = foldl (\acc (a, b) -> Map.insert a b acc)
            newMap1 = Map.insert k1 newK $ mapFold varMap1 $ zip args1 newArgs
            newMap2 = Map.insert k2 newK $ mapFold varMap2 $ zip args2 newArgs
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
    show (CLetCont k x c1 c2) = "letcont " ++ k ++ " " ++ x ++ " = (" ++ show c1 ++ ") in " ++ show c2
    show (CAppCont k x) = k ++ " " ++ x
    show (CAppFun f k args) = f ++ " " ++ k ++ " " ++ unwords args ++ ")"
    show (CLetPrim x primOp args cexpr) = "letprim " ++ x ++ " = (" ++ show primOp ++ " " ++ unwords args ++ ") in " ++ show cexpr
    show (CIf x k1 k2) = "if " ++ x ++ " then " ++ k1 ++ " else " ++ k2
    show (CLetFix f k args c1 c2) = "letfix " ++ f ++ " " ++ k ++ " " ++ unwords args ++ " = (" ++ show c1 ++ ") in " ++ show c2
    show (CExit x) = "Exit(" ++ x ++ ")"

instance Show CVal where
    show (CLitFloat f) = show f
    show (CLamCont k x cexpr) = "λ " ++ k ++ " " ++ x ++ " -> " ++ show cexpr

instance Show CPrimOp where
    show (CBinOp op) = show op
    show (CUnOp op)  = show op

instance Show CProg where
    show (CProcLam procName k args e) = 
        "letproc " ++ procName ++ " " ++ k ++ " " ++ unwords args ++ " = " ++ show e
--     show (CJumpLam args e) =
--         "λjump(" ++ intercalate ", " args ++ ") -> " ++ show e