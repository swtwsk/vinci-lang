module Core.FreeVariables (
    freeVariablesProg,
    freeVariablesExpr
) where

import Control.Monad.Reader
import qualified Data.Set as Set
import Data.Functor ((<&>))

import Core.AST

type VarSet a = Set.Set (VarId a)

freeVariablesProg :: (EquableFunctor a) => Prog a -> VarSet a
freeVariablesProg (Prog f args e) = freeVariablesExpr e (Set.fromList (f:args))

freeVariablesExpr :: (EquableFunctor a) => Expr a -> VarSet a -> VarSet a
freeVariablesExpr e = runReader (freeVariables' e)

freeVariables' :: (EquableFunctor a) => Expr a -> Reader (VarSet a) (VarSet a)
freeVariables' (Var v) = ask <&> \s -> if Set.member v s 
    then Set.empty 
    else Set.singleton v
freeVariables' (Lit _) = return Set.empty
freeVariables' (App e1 e2) = do
    fv1 <- freeVariables' e1
    fv2 <- freeVariables' e2
    return $ fv1 `Set.union` fv2
freeVariables' (If c e1 e2) = do
    fvc <- freeVariables' c
    fv1 <- freeVariables' e1
    fv2 <- freeVariables' e2
    return $ fvc `Set.union` fv1 `Set.union` fv2
freeVariables' (Cons _ exprs) = do
    fvs <- mapM freeVariables' exprs
    return $ foldl1 Set.union fvs
freeVariables' (FieldGet _ e) = freeVariables' e
freeVariables' (TupleCons exprs) = do
    fvs <- mapM freeVariables' exprs
    return $ foldl1 Set.union fvs
freeVariables' (TupleProj _i e) = freeVariables' e
freeVariables' (Let n e1 e2) = do
    fv1 <- freeVariables' e1
    fv2 <- local (Set.insert n) (freeVariables' e2)
    return $ fv1 `Set.union` fv2
freeVariables' (LetFun (Prog f args e1) e2) = do
    fv1 <- local (Set.union (Set.fromList args) . Set.insert f) (freeVariables' e1)
    fv2 <- local (Set.insert f) (freeVariables' e2)
    return $ fv1 `Set.union` fv2
freeVariables' (BinOp _ e1 e2) = do
    fv1 <- freeVariables' e1
    fv2 <- freeVariables' e2
    return $ fv1 `Set.union` fv2
freeVariables' (UnOp _ e) = freeVariables' e
