module Core.LambdaLifting (lambdaLiftProg) where
-- based on Johnson's lifting algorithm (O(n^3))
-- from "Lambda-Lifting in Quadratic Time"

import Control.Monad.Reader
import Data.Bifunctor (bimap, second)
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Core.AST
import Core.Utils (aggregateApplications)

type FunName = String
type LiftMap = Map.Map FunName [VarId Maybe]

type LiftM = Reader LiftMap

lambdaLiftProg :: Prog Maybe -> [Prog Maybe]
lambdaLiftProg = Set.toList . blockFloatDef . parameterLiftProg

-- parameter lifting
parameterLiftProg :: Prog Maybe -> Prog Maybe
parameterLiftProg p = runReader (parameterLiftDef p) Map.empty

parameterLiftDef :: Prog Maybe -> LiftM (Prog Maybe)
parameterLiftDef (Prog fName args e) = do
    e' <- parameterLiftExpr e
    applySolutionToDef (Prog fName args e')

parameterLiftExpr :: Expr Maybe -> LiftM (Expr Maybe)
parameterLiftExpr v@(Var _) = applySolutionToExpr v
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
parameterLiftExpr (TupleCons exprs) = TupleCons <$> mapM parameterLiftExpr exprs
parameterLiftExpr (TupleProj i e) = TupleProj i <$> parameterLiftExpr e
parameterLiftExpr (Let n e1 e2) = do
    e1' <- parameterLiftExpr e1
    e2' <- parameterLiftExpr e2
    return $ Let n e1' e2'
parameterLiftExpr (LetFun p@(Prog f@(VarId n _) args e1) e2) = do
    lifts <- ask
    let fv  = runReader (freeVariables e1) (Set.fromList (f:args))
        fv' = Set.toList fv
        -- we can use dummyType because Eq looks just on name of the VarId
        mergeFun acc g gVars = 
            if Set.member (VarId g $ Just TDummy) fv then acc ++ gVars else acc
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

applySolutionToDef :: Prog Maybe -> LiftM (Prog Maybe)
applySolutionToDef p@(Prog f@(VarId fName _t) args e) = 
    ask <&> \m -> case Map.lookup fName m of
        Just varList -> 
            let t' = getLiftedFunType f varList in
            Prog (VarId fName t') (varList ++ args) e
        Nothing -> p

applySolutionToExpr :: Expr Maybe -> LiftM (Expr Maybe)
applySolutionToExpr e@(Var f@(VarId fName _t)) = 
    ask <&> \m -> case Map.lookup fName m of
        Just varList -> 
            let t' = getLiftedFunType f varList in
            appFunVars (VarId fName t') varList
        Nothing -> e
applySolutionToExpr e = return e

appFunVars :: VarId Maybe -> [VarId Maybe] -> Expr Maybe
appFunVars f = foldl (\acc el -> App acc (Var el)) (Var f)

getLiftedFunType :: VarId Maybe -> [VarId Maybe] -> Maybe Type
getLiftedFunType (VarId _f (Just t)) args = Just $ foldr TFun t (fromJust . _varType <$> args)
getLiftedFunType _ _ = Nothing

freeVariables :: Expr Maybe 
    -> Reader (Set.Set (VarId Maybe)) (Set.Set (VarId Maybe))
freeVariables (Var v) = ask <&> \s -> if Set.member v s 
    then Set.empty 
    else Set.singleton v
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
freeVariables (TupleCons exprs) = do
    fvs <- mapM freeVariables exprs
    return $ foldl1 Set.union fvs
freeVariables (TupleProj _i e) = freeVariables e
freeVariables (Let n e1 e2) = do
    fv1 <- freeVariables e1
    fv2 <- local (Set.insert n) (freeVariables e2)
    return $ fv1 `Set.union` fv2
freeVariables (LetFun (Prog f args e1) e2) = do
    fv1 <- local (Set.union (Set.fromList args) . Set.insert f) (freeVariables e1)
    fv2 <- local (Set.insert f) (freeVariables e2)
    return $ fv1 `Set.union` fv2
freeVariables (BinOp _ e1 e2) = do
    fv1 <- freeVariables e1
    fv2 <- freeVariables e2
    return $ fv1 `Set.union` fv2
freeVariables (UnOp _ e) = freeVariables e

-- block floating
blockFloatDef :: Prog Maybe -> Set.Set (Prog Maybe)
blockFloatDef (Prog fName args e) = Set.insert (Prog fName args e') fs
    where (fs, e') = blockFloatExpr e

blockFloatExpr :: Expr Maybe -> (Set.Set (Prog Maybe), Expr Maybe)
blockFloatExpr v@(Var _) = (Set.empty, v)
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
blockFloatExpr (TupleCons exprs) = (s', TupleCons exprs')
    where
        (s', exprs') = extractProgs $ map blockFloatExpr exprs
        extractProgs :: [(Set.Set (Prog Maybe), Expr Maybe)] -> (Set.Set (Prog Maybe), [Expr Maybe])
        extractProgs ((s, expr):t) = 
            bimap (s `Set.union`) (expr:) $ extractProgs t
        extractProgs [] = (Set.empty, [])
blockFloatExpr (TupleProj i e) = second (TupleProj i) $ blockFloatExpr e
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
