{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Core.TypeChecking (tiCoreManager, tiCoreManagerToType, tiProgs) where

import Core.AST
import Core.CoreManager (CoreManager(..))
import Core.Types
import LibraryList (librarySchemeList)
import StructDefMap (StructDefMap)
import Utils.List ((!!?))
import Utils.Tuple

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.List (find, nub)
import qualified Data.Map as Map
import qualified Data.Set as Set

type UExpr = Expr Maybe
type TExpr = Expr Identity

type Subst = Map.Map Tyvar Type

newtype TypeEnv = TypeEnv (Map.Map String Scheme) deriving (Show)

data TCMEnv = TCMEnv { _typeEnv :: TypeEnv
                     , _structDefMap :: StructDefMap Type
                     , _classEnv :: ClassEnv }
data TCMState = TCMState { _tSupply :: Int
                         , _tSubst  :: Subst }
                         deriving (Show)
type TCM a = ReaderT TCMEnv (StateT TCMState (Except String)) a

tiCoreManager :: CoreManager Maybe -> Either String (CoreManager Identity)
tiCoreManager cm = do
    let env = TCMEnv { _typeEnv      = TypeEnv librarySchemeList
                     , _structDefMap = _structDefs cm
                     , _classEnv     = classEnv }
    typed <- tiProgsRun env (_progs cm)
    return $ CoreManager { _progs      = fst <$> typed
                         , _structDefs = _structDefs cm }

tiProgs :: [Prog Maybe] -> Either String [Prog Identity]
tiProgs = second (map fst) . tiProgsRun env
    where
        env = TCMEnv { _typeEnv      = TypeEnv librarySchemeList
                     , _structDefMap = Map.empty
                     , _classEnv     = classEnv }

tiCoreManagerToType :: CoreManager Maybe -> Either String [Type]
tiCoreManagerToType cm = do
    let env = TCMEnv { _typeEnv      = TypeEnv librarySchemeList
                     , _structDefMap = _structDefs cm
                     , _classEnv     = classEnv }
    typed <- tiProgsRun env (_progs cm)
    return $ snd <$> typed

tiProgsRun :: TCMEnv
           -> [Prog Maybe]
           -> Either String [(Prog Identity, Type)]
tiProgsRun env (p:t) = do
    (p', TypeEnv tenv, s, t') <- runTCM env (tiProg' p)
    typedProg <- either throwError return $ 
        applySubstProg (s, _structDefMap env) (Map.map fromScheme tenv) p'
    let env'  = env { _typeEnv = TypeEnv tenv }
        (_ :=> tProg) = t'
    rest <- tiProgsRun env' t
    return $ (typedProg, tProg):rest
tiProgsRun _ [] = return []

-- TODO: HANDLE RECURSION!
tiProg' :: Prog Maybe -> TCM (Prog Identity, TypeEnv, Subst, Qual Type)
tiProg' (Prog (VarId fName _fType) args e) = do
    -- returnType <- case resType (length args) fType of
    --     Just t -> return t
    --     Nothing -> throwError $ "Argument count and function type for " ++ show p ++ " is not aligned"
    args' <- forM args $ \(VarId an at) -> case at of
        Just t -> return (an, toScheme t)
        Nothing -> (\t -> (an, toScheme t)) <$> newTyVar "a"
    fTyVar <- newTyVar "f"
    tenv <- asks _typeEnv
    let TypeEnv tenv' = foldl remove tenv (fName:(fst <$> args'))
        tenv'' = tenv' `Map.union` 
                 Map.fromList args' `Map.union` 
                 Map.singleton fName (toScheme fTyVar)
    (e', s1, p1 :=> t1) <- local (\env -> env { _typeEnv = TypeEnv tenv'' }) (tiExpr e)

    let fType = foldr (\(_, sc) -> TFun (apply s1 $ fromScheme sc)) t1 args'
    s2 <- mgu fType (apply s1 fTyVar)
    let pre = p1 :=> apply s2 fType
    let TypeEnv env2 = remove tenv fName
        t' = generalize (apply s1 tenv) pre
        tenv''' = TypeEnv $ Map.insert fName t' env2
    p2 :=> finalT <- instantiate t'
    
    finalArgTypes <- extractArgTypes (length args) finalT
    let args'' = uncurry Var' <$> zip (fst <$> args') finalArgTypes

    ps <- entailOrThrow s1 (nub (p1 ++ p2))
    return (Prog (Var' fName finalT) args'' e', tenv''', s1, ps :=> finalT)
    where
        extractArgTypes :: Int -> Type -> TCM [Type]
        extractArgTypes 0 _ = return []
        extractArgTypes i t = case t of
            (TFun at rest) -> (at:) <$> extractArgTypes (i - 1) rest
            _ -> throwError "Expected function type with more arguments"

tiExpr :: UExpr -> TCM (TExpr, Subst, Qual Type)
tiExpr (Var (VarId v _)) = do  -- TODO: check type equality
    (TypeEnv env) <- asks _typeEnv
    case Map.lookup v env of
        Nothing -> throwError $ "Unbound variable: " ++ v
        Just sigma -> do
            qt@(_ :=> t) <- instantiate sigma
            return (Var $ Var' v t, nullSubst, qt)
tiExpr (Lit l) = (\(s, t) -> (Lit l, s, t)) <$> tiLit l
tiExpr a@(App e1 e2) = do
    (e1', s1, p1 :=> t1) <- tiExpr e1
    (e2', s2, p2 :=> t2) <- local (apply s1) (tiExpr e2)
    tv <- newTyVar "a"
    let errorPrefix = "In application " ++ show a ++ " "
    s3 <- mgu (TFun t2 tv) (apply s2 t1) `catchError` (throwError . (errorPrefix ++))
    let s = s3 `composeSubst` s2 `composeSubst` s1
    ps <- entailOrThrow s (nub (p1 ++ p2))
    return (App e1' e2', s, ps :=> apply s3 tv)
tiExpr i@(If c e1 e2) = do
    (c', sc, pc :=> tc) <- tiExpr c
    (e1', s1, p1 :=> t1) <- tiExpr e1
    (e2', s2, p2 :=> t2) <- tiExpr e2
    let errorPrefix = "In " ++ show i ++ " "
    s3 <- mgu t1 t2 `catchError` (throwError . (errorPrefix ++))
    s4 <- mgu tc TBool `catchError` (throwError . (errorPrefix ++))
    let s = s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1 `composeSubst` sc
    ps <- entailOrThrow s (nub (pc ++ p1 ++ p2))
    return (If c' e1' e2', s, ps :=> apply s t1)
tiExpr c@(Cons sName exprs) = do
    (texprs, s1, qtypes) <- tiExprs exprs
    structTypes <- asks _structDefMap
    fieldTypes <- case Map.lookup sName structTypes of
        Just fds -> return $ trdTriple <$> fds
        Nothing -> throwError $ "Undefined struct " ++ sName
    let zippedTypes = zip qtypes fieldTypes
    s2 <- foldM foldFn s1 zippedTypes
    let s = s2 `composeSubst` s1
    ps <- entailOrThrow s . nub $ qtypes >>= (\(p :=> _) -> p)
    return (Cons sName texprs, s, ps :=> TStruct sName)
    where
        errorPrefix = "In " ++ show c ++ " "
        foldFn s (_ :=> t, ft) = (`composeSubst` s) <$> 
            mgu (apply s t) ft `catchError` (throwError . (errorPrefix ++))
tiExpr (FieldGet fName e) = do
    (texpr, s, qt@(_ :=> t)) <- tiExpr e
    case t of
        TStruct sName -> structGetter sName texpr s qt
        TTuple _ _ -> tupleIndex >>= \i -> tiExpr (TupleProj i e)
        TVar _ -> throwError $ "Give explicit type annotations for variables used in " ++ show e
        _ -> throwError $ "Field getter: Expected struct, got " ++ show t ++ " instead"
    where
        structGetter :: String 
                     -> TExpr 
                     -> Subst 
                     -> Qual Type 
                     -> TCM (TExpr, Subst, Qual Type)
        structGetter sName texpr s (p :=> _) = do
            structTypes <- asks _structDefMap
            fieldDefs <- case Map.lookup sName structTypes of
                Just fds -> return fds
                Nothing -> throwError $ "Undefined struct " ++ sName
            (_, _, fieldType) <- case find ((== fName) . fstTriple) fieldDefs of
                Just fieldType -> return fieldType
                Nothing -> throwError $ 
                    "No field `" ++ fName ++ "` on type `" ++ sName ++ "`"
            return (FieldGet fName texpr, s, p :=> fieldType)
        tupleIndex :: TCM Int
        tupleIndex | fName == "x" || fName == "r" = return 0
                   | fName == "y" || fName == "g" = return 1
                   | fName == "z" || fName == "b" = return 2
                   | fName == "w" || fName == "a" = return 3
                   | otherwise = throwError $ "Tuple " ++ show e ++ " has too few fields"
tiExpr tc@(TupleCons exprs) = do
    (texprs, s1, qtypes) <- tiExprs exprs
    let headType:restTypes = (\(_ :=> t) -> t) <$> qtypes
    s2 <- foldM (foldFn headType) s1 restTypes
    ps <- entailOrThrow s2 . nub $ qtypes >>= (\(p :=> _) -> p)
    let tupleConsType = ps :=> TTuple (apply s2 headType) (length exprs)
    return (TupleCons texprs, s2, tupleConsType)
    where
        errorPrefix = "In " ++ show tc ++ " "
        foldFn headType s t = (`composeSubst` s) <$> 
            mgu (apply s headType) t `catchError` (throwError . (errorPrefix ++))
tiExpr p@(TupleProj i e) = do
    (texpr, s, ps :=> t) <- tiExpr e
    case t of
        TTuple t' size -> if i <= size
            then return (TupleProj i texpr, s, ps :=> apply s t')
            else throwError $ "Tuple " ++ show e ++ " has too few fields"
        TStruct sName -> do
            structTypes <- asks _structDefMap
            fieldDefs <- case Map.lookup sName structTypes of
                Just fds -> return fds
                Nothing -> throwError $ "Undefined struct " ++ sName
            fieldName <- case fieldDefs !!? i of
                Just (fieldName, _, _) -> return fieldName
                Nothing -> throwError $ "Struct `" ++ sName ++ "` has too few fields"
            tiExpr (FieldGet fieldName e)
        TVar _ -> throwError $ "Give explicit type annotations for variables used in " ++ show e
        _ -> throwError $ "In " ++ show p ++ " expected tuple, got " ++ 
            show e ++ " of type " ++ show t ++ " instead"
tiExpr (Let (VarId x _) e1 e2) = do
    (e1', s1, pre@(p1 :=> t1)) <- tiExpr e1
    env <- asks _typeEnv
    let TypeEnv env' = remove env x
        t' = generalize (apply s1 env) pre
        env'' = TypeEnv (Map.insert x t' env')
    (e2', s2, p2 :=> t2) <- local (\e -> e { _typeEnv = apply s1 env''}) (tiExpr e2)
    let s = s2 `composeSubst` s1
    ps <- entailOrThrow s (nub (p1 ++ p2))
    return (Let (Var' x t1) e1' e2', s, ps :=> t2)
tiExpr (LetFun p e) = do
    (p', tenv, sp, pp :=> _tp) <- tiProg' p
    (e', se, pe :=> te) <- local (\env -> env { _typeEnv = apply sp tenv }) (tiExpr e)
    let s = se `composeSubst` sp
    ps <- entailOrThrow s (nub (pp ++ pe))
    return (LetFun p' e', s, ps :=> te)
tiExpr (BinOp op e1 e2) = do
    (e', s, t) <- tiExpr (App (App (Var $ VarId (show op) Nothing) e1) e2)
    let App (App _ e1') e2' = e'
    return (BinOp op e1' e2', s, t)
tiExpr (UnOp op e) = do
    (e', s, t) <- tiExpr (App (Var $ VarId (show op) Nothing) e)
    let App _ e'' = e'
    return (UnOp op e'', s, t)

tiLit :: Lit -> TCM (Subst, Qual Type)
tiLit (LFloat _) = return (nullSubst, [] :=> TFloat)
tiLit (LBool _)  = return (nullSubst, [] :=> TBool)
tiLit (LInt _)   = return (nullSubst, [] :=> TInt)

tiExprs :: [UExpr] -> TCM ([TExpr], Subst, [Qual Type])
tiExprs exprs = do
    (texprs, s, qtypes) <- foldM foldFn ([], nullSubst, []) exprs
    return (reverse texprs, s, reverse qtypes)
    where
        foldFn (texprs, s1, types) expr = do
            (texpr, s2, qt) <- local (apply s1) (tiExpr expr)
            return (texpr:texprs, s2 `composeSubst` s1, qt:types)

type ApplyEnv = Map.Map String Type
type ApplyM = ReaderT (Subst, StructDefMap Type) (StateT ApplyEnv (Except String))

applySubstProg :: (Subst, StructDefMap Type) -> ApplyEnv -> Prog Identity -> Either String (Prog Identity)
applySubstProg s applyEnv p = second fst $ 
    runExcept (evalStateT (runReaderT (applySubstProg' p) s) applyEnv)

applySubstProg' :: Prog Identity -> ApplyM (Prog Identity, Type)
applySubstProg' (Prog (VarId fName (Identity fType)) args e) = do
    s <- asks fst
    let fType' = apply s fType
        appliedArgs = (\(Var' an at) -> (an, apply s at)) <$> args
        args' = uncurry Var' <$> appliedArgs
    modify (Map.insert fName fType' . Map.union (Map.fromList appliedArgs))
    (e', _) <- applySubstExpr e
    return (Prog (Var' fName fType') args' e', fType)

applySubstExpr :: TExpr -> ApplyM (TExpr, Type)
applySubstExpr (Var (VarId v _)) = do
    t <- gets $ Map.lookup v
    case t of
        Nothing -> throwError $ "Variable " ++ v ++ " hasn't been given a type"
        Just t' -> return (Var . Var' v $ t', t')
applySubstExpr (Lit l) = return . (Lit l, ) $ case l of
    LFloat _ -> TFloat
    LBool _ -> TBool
    LInt _ -> TInt
applySubstExpr (App e1 e2) = do
    (e1', t) <- applySubstExpr e1
    (e2', _) <- applySubstExpr e2
    let TFun _ t' = t
    return (App e1' e2', t')
applySubstExpr (If c e1 e2) = do
    (c', _)  <- applySubstExpr c
    (e1', t) <- applySubstExpr e1
    (e2', _) <- applySubstExpr e2
    return (If c' e1' e2', t)
applySubstExpr (Cons sName eList) = do
    eList' <- mapM applySubstExpr eList
    return (Cons sName (fst <$> eList'), TStruct sName) 
applySubstExpr (FieldGet fName e) = do
    (e', t) <- applySubstExpr e
    sName <- case t of
        TStruct sName -> return sName
        _ -> throwError $ "Expected struct type for " ++ show e ++ ", got " ++ show t ++ " instead"
    structTypes <- asks snd
    let fieldDefs = structTypes Map.! sName
    (_, _, fieldType) <- case find ((== fName) . fstTriple) fieldDefs of
        Just fieldType -> return fieldType
        Nothing -> undefined
    return (FieldGet fName e', fieldType)
applySubstExpr (TupleCons eList) = do
    eList' <- mapM applySubstExpr eList
    return ( TupleCons (fst <$> eList')
           , TTuple (head $ snd <$> eList') (length eList') )
applySubstExpr (TupleProj i e) = do
    (e', t) <- applySubstExpr e
    let TTuple t' _ = t
    return (TupleProj i e', t')
applySubstExpr (Let (VarId v _) e1 e2) = do
    (e1', t) <- applySubstExpr e1
    t' <- asks $ (`apply` t) . fst
    modify (Map.insert v t')
    (e2', t2) <- applySubstExpr e2
    return (Let (Var' v t') e1' e2', t2)
applySubstExpr (LetFun p@(Prog (VarId fName _) _ _) e) = do
    (p', tp) <- applySubstProg' p
    modify $ Map.insert fName tp
    (e', te) <- applySubstExpr e
    return (LetFun p' e', te)
applySubstExpr (BinOp op e1 e2) = do
    (e1', t1) <- applySubstExpr e1
    (e2', t2) <- applySubstExpr e2
    let env = librarySchemeList
        Scheme _ (_ :=> TFun ft1 (TFun ft2 ft3)) = env Map.! show op
        retType = case (ft1, ft2, ft3) of
            (TVar (Tyvar a), TVar _, TVar (Tyvar c)) -> 
                if a == c then t1 else t2
            (TVar _, _, TVar _) -> t1
            (_, TVar _, TVar _) -> t2
            (_, _, t) -> t
    return (BinOp op e1' e2', retType)
applySubstExpr (UnOp op e) = do
    (e', te) <- applySubstExpr e
    let env = librarySchemeList
        Scheme _ (_ :=> TFun ft1 ft2) = env Map.! show op
        retType = case (ft1, ft2) of
            (TVar _, TVar _) -> te
            (_, t) -> t
    return (UnOp op e', retType)

runTCM :: TCMEnv -> TCM a -> Either String a
runTCM env t = runExcept (evalStateT (runReaderT t env) initTIState)
    where
        initTIState = TCMState { _tSupply = 0, _tSubst = Map.empty }

newTyVar :: String -> TCM Type
newTyVar prefix = do
    s <- get
    put $ s { _tSupply = _tSupply s + 1 }
    return (TVar . Tyvar $ prefix ++ show (_tSupply s))

-- SUBSTS, TYPES, MGU
nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

class Types a where
    apply :: Subst -> a -> a
    ftv   :: a -> Set.Set Tyvar

instance Types Type where
    apply s (TVar u)     = case Map.lookup u s of
        Just t -> t
        Nothing -> TVar u
    apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)
    apply _ t            = t

    ftv (TVar n)     = Set.singleton n
    ftv (TFun t1 t2) = ftv t1 `Set.union` ftv t2
    ftv _            = Set.empty

instance Types a => Types [a] where
    apply s = map (apply s)
    ftv     = foldr (Set.union . ftv) Set.empty

instance Types t => Types (Qual t) where
    apply s (ps :=> t) = apply s ps :=> apply s t
    ftv (ps :=> t)     = ftv ps `Set.union` ftv t

instance Types Pred where
    apply s (IsIn i t) = IsIn i (apply s t)
    ftv (IsIn _ t)     = ftv t

instance Types Scheme where
    apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)
    ftv (Scheme vars t)     = ftv t Set.\\ Set.fromList vars

instance Types TCMEnv where
    apply s tenv = tenv { _typeEnv = apply s (_typeEnv tenv) }
    ftv = ftv . _typeEnv

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

instance Types TypeEnv where
    apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)
    ftv (TypeEnv env)     = ftv (Map.elems env)

generalize :: TypeEnv -> Qual Type -> Scheme
generalize env t = Scheme vars t
    where
        vars = Set.toList (ftv t `Set.difference` ftv env)

instantiate :: Scheme -> TCM (Qual Type)
instantiate (Scheme vars t) = do
    nvars <- mapM (\_ -> newTyVar "a") vars
    let s = Map.fromList (zip vars nvars)
    return $ apply s t

mgu :: Type -> Type -> TCM Subst
mgu (TFun l r) (TFun l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return (s1 `composeSubst` s2)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu t1 t2
    | t1 == t2 = return nullSubst
    | otherwise = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2

varBind :: Tyvar -> Type -> TCM Subst
varBind u t
    | t == TVar u = return nullSubst
    | u `Set.member` ftv t = throwError $ "occur check fails: " ++ show u ++ " vs. " ++ show t
    | otherwise   = return (Map.singleton u t)

entailOrThrow :: Subst -> [Pred] -> TCM [Pred]
entailOrThrow s preds = do
    let substPreds = apply s preds
    preds' <- forM substPreds $ \p@(IsIn c t) -> do
        (_, types) <- asks (Map.lookup c . _classEnv) >>= \case
            Just classDef -> return classDef
            Nothing -> throwError $ "Unexpected class " ++ show c
        case t of
            TVar _ -> return [p]
            TFun _ _ -> return [p] -- ?
            tc -> if tc `elem` types then return [] else throwError $ 
                        show tc ++ " is not a member of class " ++ show c
    return $ concat preds'
