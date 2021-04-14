module Core.LambdaLifting (lambdaLiftProg) where
-- based on Johnson's lifting algorithm (O(n^3))
-- from "Lambda-Lifting in Quadratic Time"

import Control.Monad.Reader
import Data.Bifunctor (second)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import qualified Data.Set as Set

import Core.AST
import Core.Utils (aggregateApplications)

type FunName = String
type Var = String
type LiftMap = Map.Map FunName [Var] --(Set.Set Var)

type LiftM = Reader LiftMap

lambdaLiftProg :: Prog -> [Prog]
lambdaLiftProg = Set.toList . blockFloatDef . parameterLiftProg

-- parameter lifting
parameterLiftProg :: Prog -> Prog
parameterLiftProg p = runReader (parameterLiftDef p) Map.empty

parameterLiftDef :: Prog -> LiftM Prog
parameterLiftDef (Prog t fName args e) = do
    e' <- parameterLiftExpr e
    applySolutionToDef (Prog t fName args e')

parameterLiftExpr :: Expr -> LiftM Expr
parameterLiftExpr v@(Var _) = applySolutionToExpr v
parameterLiftExpr (Lam _n _e) = undefined  -- TODO: no lambdas
parameterLiftExpr l@(Lit _) = return l
parameterLiftExpr e@App {} = do
    let (f, args) = aggregateApplications e
    f' <- parameterLiftExpr f
    args' <- mapM parameterLiftExpr args
    return $ foldl App f' args'
parameterLiftExpr (If c e1 e2) = do
    c'  <- parameterLiftExpr c
    e1' <- parameterLiftExpr e1
    e2' <- parameterLiftExpr e2
    return $ If c' e1' e2'
parameterLiftExpr (Let n e1 e2) = do
    e1' <- parameterLiftExpr e1
    e2' <- parameterLiftExpr e2
    return $ Let n e1' e2'
parameterLiftExpr (LetFun p@(Prog progType n args e1) e2) = do
    lifts <- ask
    let args' = case progType of { NonRec -> args ; Rec -> n:args }
        fv  = runReader (freeVariables e1) (Set.fromList args')
        fv' = Set.toList fv
        mergeFun acc g gVars = if Set.member g fv then acc ++ gVars else acc
        fv''   = Map.foldlWithKey mergeFun fv' lifts
        lifts' = Map.insert n fv'' lifts
    p'  <- local (const lifts') (parameterLiftDef p)
    e2' <- local (const lifts') (parameterLiftExpr e2)
    return $ LetFun p' e2'
parameterLiftExpr (BinOp op e1 e2) = do
    e1' <- parameterLiftExpr e1
    e2' <- parameterLiftExpr e2
    return $ BinOp op e1' e2'
parameterLiftExpr (UnOp op e) = UnOp op <$> parameterLiftExpr e

applySolutionToDef :: Prog -> LiftM Prog
applySolutionToDef p@(Prog t f args e) = ask <&> \m -> case Map.lookup f m of
    Just varList -> Prog t f (varList ++ args) e
    Nothing -> p

applySolutionToExpr :: Expr -> LiftM Expr
applySolutionToExpr e@(Var f) = ask <&> \m -> case Map.lookup f m of
    Just varList -> appFunVars f varList
    Nothing -> e
applySolutionToExpr e = return e

-- block floating
blockFloatDef :: Prog -> Set.Set Prog
blockFloatDef (Prog t fName args e) = Set.insert (Prog t fName args e') fs
    where (fs, e') = blockFloatExpr e

blockFloatExpr :: Expr -> (Set.Set Prog, Expr)
blockFloatExpr v@(Var _) = (Set.empty, v)
blockFloatExpr (Lam _n _e) = undefined
blockFloatExpr l@(Lit _) = (Set.empty, l)
blockFloatExpr (App e1 e2) = (s1 `Set.union` s2, App e1' e2')
    where
        (s1, e1') = blockFloatExpr e1
        (s2, e2') = blockFloatExpr e2
blockFloatExpr (If c e1 e2) = (sc `Set.union` s1 `Set.union` s2, If c' e1' e2')
    where
        (sc, c')  = blockFloatExpr c
        (s1, e1') = blockFloatExpr e1
        (s2, e2') = blockFloatExpr e2
blockFloatExpr (Let n e1 e2) = (s1 `Set.union` s2, Let n e1' e2')
    where
        (s1, e1') = blockFloatExpr e1
        (s2, e2') = blockFloatExpr e2
blockFloatExpr (LetFun p e) = (sp `Set.union` se, e')
    where
        sp = blockFloatDef p
        (se, e') = blockFloatExpr e
blockFloatExpr (BinOp op e1 e2) = (s1 `Set.union` s2, BinOp op e1' e2')
    where
        (s1, e1') = blockFloatExpr e1
        (s2, e2') = blockFloatExpr e2
blockFloatExpr (UnOp op e) = second (UnOp op) $ blockFloatExpr e

appFunVars :: FunName -> [Var] -> Expr
appFunVars f = foldl (\acc el -> App acc (Var el)) (Var f)

freeVariables :: Expr -> Reader (Set.Set Var) (Set.Set Var)
freeVariables (Var v) = ask <&> \s -> if Set.member v s 
    then Set.empty 
    else Set.singleton v
freeVariables (Lam _n _e) = undefined
freeVariables (Lit _) = return Set.empty
freeVariables (App e1 e2) = do
    fv1 <- freeVariables e1
    fv2 <- freeVariables e2
    return $ fv1 `Set.union` fv2
freeVariables (If c e1 e2) = do
    fvc <- freeVariables c
    fv1 <- freeVariables e1
    fv2 <- freeVariables e2
    return $ fvc `Set.union` fv1 `Set.union` fv2
freeVariables (Let n e1 e2) = do
    fv1 <- freeVariables e1
    fv2 <- local (Set.insert n) (freeVariables e2)
    return $ fv1 `Set.union` fv2
freeVariables (LetFun (Prog NonRec f args e1) e2) = do
    fv1 <- local (Set.union (Set.fromList args)) (freeVariables e1)
    fv2 <- local (Set.insert f) (freeVariables e2)
    return $ fv1 `Set.union` fv2
freeVariables (LetFun (Prog Rec f args e1) e2) = do
    fv1 <- local (Set.union (Set.fromList args) . Set.insert f) (freeVariables e1)
    fv2 <- local (Set.insert f) (freeVariables e2)
    return $ fv1 `Set.union` fv2
freeVariables (BinOp _ e1 e2) = do
    fv1 <- freeVariables e1
    fv2 <- freeVariables e2
    return $ fv1 `Set.union` fv2
freeVariables (UnOp _ e) = freeVariables e
