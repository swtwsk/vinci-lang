module Core.LambdaLifting (
    lambdaLiftProgs
) where
-- based on Johnson's lifting algorithm (O(n^3))
-- from "Lambda-Lifting in Quadratic Time"

import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader
import Data.Bifunctor (bimap, second)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import qualified Data.Set as Set

import Core.AST
import Core.FreeVariables (freeVariablesExpr)
import Core.Utils (aggregateApplications)
import LibraryList (coreLibraryList)

type FunName = String
type LiftMap = Map.Map FunName [VarId Identity]
data LiftEnv = LiftEnv { _globals :: Set.Set (VarId Identity)
                       , _lifts   :: LiftMap }
type LiftM = Reader LiftEnv

lambdaLiftProgs :: [Prog Identity] -> [Prog Identity]
lambdaLiftProgs progs = progs >>= lambdaLiftProg'
    where
        libraryGlobals = Set.fromList $ (`Var'` TDummy) . fst <$> Map.toList coreLibraryList
        progGlobals = Set.fromList $ (\(Prog f _ _) -> f) <$> progs
        globals = Set.union libraryGlobals progGlobals
        lambdaLiftProg' = Set.toList . blockFloatDef . parameterLiftProg globals

-- parameter lifting
parameterLiftProg :: Set.Set (VarId Identity) -> Prog Identity -> Prog Identity
parameterLiftProg globals prog = runReader (parameterLiftDef prog) env
    where
        env = LiftEnv { _globals = globals, _lifts = Map.empty }

parameterLiftDef :: Prog Identity -> LiftM (Prog Identity)
parameterLiftDef (Prog fName args e) = do
    e' <- parameterLiftExpr e
    applySolutionToDef (Prog fName args e')

parameterLiftExpr :: Expr Identity -> LiftM (Expr Identity)
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
    LiftEnv globals lifts <- ask
    let fv  = freeVariablesExpr e1 (Set.union (Set.fromList (f:args)) globals)
        fv' = Set.toList fv
        -- we can use dummyType because Eq looks just on name of the VarId
        mergeFun acc g gVars = 
            if Set.member (Var' g TDummy) fv then acc ++ gVars else acc
        fv''   = Map.foldlWithKey mergeFun fv' lifts
        lifts' = Map.insert n fv'' lifts
    p'  <- local (\r -> r { _lifts = lifts' }) (parameterLiftDef p)
    e2' <- local (\r -> r { _lifts = lifts' }) (parameterLiftExpr e2)
    return $ LetFun p' e2'
parameterLiftExpr (BinOp op e1 e2) = do
    e1' <- parameterLiftExpr e1
    e2' <- parameterLiftExpr e2
    return $ BinOp op e1' e2'
parameterLiftExpr (UnOp op e) = UnOp op <$> parameterLiftExpr e

applySolutionToDef :: Prog Identity -> LiftM (Prog Identity)
applySolutionToDef p@(Prog f@(VarId fName _t) args e) = 
    ask <&> \(LiftEnv _ m) -> case Map.lookup fName m of
        Just varList -> 
            let t' = getLiftedFunType f varList in
            Prog (Var' fName t') (varList ++ args) e
        Nothing -> p

applySolutionToExpr :: Expr Identity -> LiftM (Expr Identity)
applySolutionToExpr e@(Var f@(VarId fName _t)) = 
    ask <&> \(LiftEnv _ m) -> case Map.lookup fName m of
        Just varList -> 
            let t' = getLiftedFunType f varList in
            appFunVars (Var' fName t') varList
        Nothing -> e
applySolutionToExpr e = return e

appFunVars :: VarId Identity -> [VarId Identity] -> Expr Identity
appFunVars f = foldl (\acc el -> App acc (Var el)) (Var f)

getLiftedFunType :: VarId Identity -> [VarId Identity] -> Type
getLiftedFunType (VarId _f (Identity t)) args = 
    foldr TFun t ((\(Identity t') -> t') . _varType <$> args)

-- block floating
blockFloatDef :: Prog Identity -> Set.Set (Prog Identity)
blockFloatDef (Prog fName args e) = Set.insert (Prog fName args e') fs
    where (fs, e') = blockFloatExpr e

blockFloatExpr :: Expr Identity -> (Set.Set (Prog Identity), Expr Identity)
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
        extractProgs :: [(Set.Set (Prog Identity), Expr Identity)] -> (Set.Set (Prog Identity), [Expr Identity])
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
