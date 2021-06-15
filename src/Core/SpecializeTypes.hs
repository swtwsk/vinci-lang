{-# LANGUAGE LambdaCase #-}
module Core.SpecializeTypes (specializeTypes) where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map as Map

import Core.AST
import qualified Core.CoreManager as CM
import Core.Ops
import Core.Types
import Core.Utils
import LibraryList (coreLibraryList)
import StructDefMap
import Utils.Tuple

type Prog' = Prog Identity
type Expr' = Expr Identity

type TyVarRenameMap = Map.Map Tyvar Type
data ReaderEnv = ReaderEnv { _tyVarMap     :: TyVarRenameMap
                           , _funs         :: Map.Map String Prog'
                           , _structDefMap :: StructDefMap Type }
type SpecializedMap = Map.Map (String, [Type]) Prog'
type SpecializeM = ReaderT ReaderEnv (State SpecializedMap)

specializeTypes :: CM.CoreManager Identity -> CM.CoreManager Identity
specializeTypes cm =
    let (mains, progsMap) = run $ do
            let specializeProg' (Prog v args e) = 
                    Prog v args . fst <$> specializeExpr e
            fragProg <- mapM specializeProg' frag
            vertProg <- mapM specializeProg' vert
            return $ catMaybes [fragProg, vertProg]
        mainsMap = Map.fromList $ (\p -> ((progId p, []), p)) <$> mains
    in cm { CM._progs = filterProgMap (mainsMap `Map.union` progsMap) }
    where
        run f      = runState (runReaderT f initialEnv) initialSt
        initialEnv = ReaderEnv 
            { _tyVarMap = Map.empty
            , _funs = Map.fromList $ (\p -> (progId p, p)) <$> CM._progs cm
            , _structDefMap = CM._structDefs cm 
            }
        initialSt  = Map.empty

        findMain name = find (\p -> progId p == name) (CM._progs cm)
        frag = findMain "frag"
        vert = findMain "vert"

-- HEURESIS: We are searching for the Prog definitions with the biggest number
-- of type arguments set (the most specific ones)
filterProgMap :: SpecializedMap -> [Prog']
filterProgMap = (snd . snd) <=< (Map.toList . Map.foldlWithKey foldFn Map.empty)
    where
        foldFn acc (fName, types) prog = 
            let typesLen = length types in
            case Map.lookup fName acc of
                Nothing -> Map.insert fName (typesLen, [prog]) acc
                Just (lt, progs) -> if lt < typesLen
                    then Map.insert fName (typesLen, [prog]) acc
                    else if lt > typesLen 
                        then acc
                        else Map.insert fName (lt, prog:progs) acc

specializeProg :: [Type] -> Prog' -> SpecializeM (VarId Identity, Type)
specializeProg argTypes (Prog (VarId fName (Identity fType)) args e) = do
    let (argPairs, tyVarRenames) = unifyTypes args argTypes
        (fType', suffix) = specializeFunctionType tyVarRenames fType
        progMapKey = (fName, Map.elems tyVarRenames)
    specializedProg <- gets $ Map.lookup progMapKey
    case specializedProg of
        Just (Prog pId _ _) -> return (pId, fType')
        Nothing -> do
            (e', _) <- local (\r -> r { _tyVarMap = tyVarRenames }) (specializeExpr e)
            let progVar = Var' (fName ++ suffix) fType'
                prog' = Prog progVar (uncurry Var' <$> argPairs) e'
            modify $ Map.insert progMapKey prog'
            return (progVar, fType')

specializeExpr :: Expr' -> SpecializeM (Expr', Type)
specializeExpr (Var var) = first Var <$> specializeVar var
specializeExpr lit@(Lit l) = case l of
    LInt _ -> return (lit, TInt)
    LFloat _ -> return (lit, TFloat)
    LBool _ -> return (lit, TBool)
specializeExpr e@App {} = do
    let (f, args) = aggregateApplications e
    (args', types) <- mapAndUnzipM specializeExpr args
    let Var (Var' fName _) = f -- ?
    (f', fType) <- case Map.lookup fName coreLibraryList of
        Just t -> 
            let fType = unifyLibFunType t types Map.empty in
            return (Var (Var' fName fType), fType)
        Nothing -> do
            prog <- asks $ (Map.! fName) . _funs
            (f', fType) <- first Var <$> specializeProg types prog
            return (f', fType)
    let (Just fType') = resType (length args) fType
    return (foldl App f' args', fType')
    where
        unifyLibFunType :: Type -> [Type] -> TyVarRenameMap -> Type
        unifyLibFunType (TFun t1 t2) (h:t) rs =
            let (tyvar, t1') = unify t1 h in
            let rs' = maybe rs (\tv -> Map.insert tv t1' rs) tyvar in
            TFun t1' (unifyLibFunType t2 t rs')
        unifyLibFunType t1 [t2] _ = snd $ unify t1 t2
        unifyLibFunType t [] rs = case t of
            TVar tyvar -> rs Map.! tyvar
            TFun (TVar tv1) (TVar tv2) -> TFun (rs Map.! tv1) (rs Map.! tv2)
            TFun (TVar tv1) t2 -> TFun (rs Map.! tv1) t2
            TFun t1 (TVar tv2) -> TFun t1 (rs Map.! tv2)
            _ -> t
        unifyLibFunType _ _ _ = undefined
specializeExpr (If c e1 e2) = do
    (c', _) <- specializeExpr c
    (e1', t) <- specializeExpr e1
    (e2', _) <- specializeExpr e2
    return (If c' e1' e2', t)
specializeExpr (Cons c exprs) = do
    (exprs', _) <- mapAndUnzipM specializeExpr exprs
    return (Cons c exprs', TStruct c)
specializeExpr (FieldGet fName e) = do
    (e', t) <- specializeExpr e
    let TStruct sName = t
    fieldDefs <- asks $ (Map.! sName) . _structDefMap
    let (_, _, ft) = fromMaybe undefined $ find ((== fName) . fstTriple) fieldDefs
    return (FieldGet fName e', ft)
specializeExpr (TupleCons exprs) = do
    (exprs', types) <- mapAndUnzipM specializeExpr exprs
    let t = case head types of
            TTuple t' _ -> TMatrix t' (length exprs')
            t' -> TTuple t' (length exprs')
    return (TupleCons exprs', t)
specializeExpr (TupleProj i e) = do
    (e', t) <- specializeExpr e
    let t' = case t of
            TTuple ti _ -> ti
            TMatrix ti size -> TTuple ti size
            _ -> undefined
    return (TupleProj i e', t')
specializeExpr (Let var e1 e2) = do
    (var', t) <- specializeVar var
    (e1', _) <- specializeExpr e1
    (e2', _) <- specializeExpr e2
    return (Let var' e1' e2', t)
specializeExpr (BinOp op e1 e2) = do
    (e1', t1) <- specializeExpr e1
    (e2', _)  <- specializeExpr e2
    let t = case op of
            OpEq -> TBool
            OpNotEq -> TBool
            OpLT -> TBool
            OpLTEq -> TBool
            OpGT -> TBool
            OpGTEq -> TBool
            _ -> t1
    return (BinOp op e1' e2', t)
specializeExpr (UnOp op e) = first (UnOp op) <$> specializeExpr e
specializeExpr (LetFun _p _e) = undefined -- it's done after lambda lifting

specializeVar :: VarId Identity -> SpecializeM (VarId Identity, Type)
specializeVar var@(VarId v (Identity t)) = case t of
    TVar tv -> do
        newType <- asks $ (Map.! tv) . _tyVarMap
        return (VarId v $ Identity newType, newType)
    _ -> return (var, t)

specializeFunctionType :: TyVarRenameMap -> Type -> (Type, String)
specializeFunctionType tvs (TFun (TVar tv) t2) = 
    let (t2', suffix) = specializeFunctionType tvs t2 in
    case Map.lookup tv tvs of
        Nothing -> undefined
        Just t  -> (TFun t t2', "_T" ++ specializedTypeSuffix t ++ suffix)
specializeFunctionType tvs (TFun t1 t2) =
    let (t1', suffix1) = specializeFunctionType tvs t1
        (t2', suffix2) = specializeFunctionType tvs t2 in
    (TFun t1' t2', suffix1 ++ suffix2)
specializeFunctionType tvs (TVar tv) = case Map.lookup tv tvs of
    Nothing -> undefined
    Just t  -> (t, "_T" ++ specializedTypeSuffix t)
specializeFunctionType tvs (TTuple t i) = 
    let (t', suffix) = specializeFunctionType tvs t in
    (TTuple t' i, suffix)
specializeFunctionType tvs (TMatrix t i) = 
    let (t', suffix) = specializeFunctionType tvs t in
    (TMatrix t' i, suffix)
specializeFunctionType _ t = (t, "")

specializedTypeSuffix :: Type -> String
specializedTypeSuffix TInt = "Int"
specializedTypeSuffix TBool = "Bool"
specializedTypeSuffix TFloat = "Float"
specializedTypeSuffix (TTuple t i) = "T" ++ specializedTypeSuffix t ++ show i
specializedTypeSuffix (TMatrix t i) = "M" ++ specializedTypeSuffix t ++ show i
specializedTypeSuffix (TStruct sName) = "S" ++ sName
specializedTypeSuffix t = error $ "Cannot create specialized suffix for " ++ show t

unifyTypes :: [VarId Identity] -> [Type] -> ([(VarName, Type)], TyVarRenameMap)
unifyTypes (VarId v (Identity vt1):vars) (vt2:types) = 
    let (typedVars, renames) = unifyTypes vars types 
        (tyvar, unified)     = unify vt1 vt2 
        renames' = maybe renames (\tv -> Map.insert tv unified renames) tyvar in
    ((v, unified):typedVars, renames')
unifyTypes [] [] = ([], Map.empty)
unifyTypes [] _  = undefined
unifyTypes _ []  = undefined

unify :: Type -> Type -> (Maybe Tyvar, Type)
unify (TVar _) (TVar _) = undefined
unify (TVar tv1) t2 = (Just tv1, t2)
unify (TTuple t1 i1) (TTuple t2 i2) 
    | i1 == i2 = second (`TTuple` i1) (unify t1 t2)
    | otherwise = undefined
unify (TMatrix t1 i1) (TMatrix t2 i2) 
    | i1 == i2 = second (`TMatrix` i1) (unify t1 t2)
    | otherwise = undefined
unify t1 t2 | t1 == t2  = (Nothing, t1)
            | otherwise = undefined
