module Core.TypeChecking where

import Core.AST
import Core.CoreManager (CoreManager(..))
import Core.Ops
import LibraryList (coreLibraryList)
import StructDefMap (StructDefMap)
import Utils.List ((!!?))
import Utils.Tuple

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor (first, second)
import Data.List (find)
import qualified Data.Map as Map

type Err = String
type TypeEnv = Map.Map String Type
type TCM = ReaderT (TypeEnv, StructDefMap Type) (Except Err)

type UExpr = Expr Maybe
type TExpr = Expr Identity

emptyEnv :: TypeEnv
emptyEnv = coreLibraryList

tcCoreManager :: CoreManager Maybe -> Either Err (CoreManager Identity)
tcCoreManager cm = do
    typed <- tcProgsRun (emptyEnv, _structDefs cm) (_progs cm)
    return $ CoreManager { _progs = typed
                         , _structDefs = _structDefs cm }

tcProgs :: [Prog Maybe] -> Either Err [Prog Identity]
tcProgs = tcProgsRun (emptyEnv, Map.empty)

tcProgsRun :: (TypeEnv, StructDefMap Type) 
           -> [Prog Maybe] 
           -> Either Err [Prog Identity]
tcProgsRun (tenv, structDefs) (p:t) = do
    (p', tProg) <- runExcept $ runReaderT (tcProg' p) (tenv, structDefs)
    let pName = progId p
        tenv' = Map.insert pName tProg tenv
    rest <- tcProgsRun (tenv', structDefs) t
    return (p':rest)
tcProgsRun _ [] = return []

tcProg :: Prog Maybe -> Either Err (Prog Identity)
tcProg p = second fst run
    where
        run = runExcept $ runReaderT (tcProg' p) (emptyEnv, Map.empty)

tc :: UExpr -> Either Err TExpr
tc e = second fst run
    where
        run = runExcept $ runReaderT (tc' e) (emptyEnv, Map.empty)

tcProg' :: Prog Maybe -> TCM (Prog Identity, Type)
tcProg' p@(Prog (VarId fName (Just fType)) args e) = do
    args' <- case sequence (_varType <$> args) of
            Just types -> return types
            Nothing -> throwError $ "Expected that " ++ show args ++ " will have explicit types annotations"
    let typedArgs = first _varName <$> zip args args'
        args''    = uncurry Var' <$> typedArgs
        argMap    = Map.fromList typedArgs
    (e', t') <- local (first $ Map.union argMap . Map.insert fName fType) (tc' e)
    returnType <- case resType (length args) fType of
        Just t -> return t
        Nothing -> throwError $ "Argument count and function type for " ++ show p ++ " is not aligned"
    _ <- checkEqOrThrow returnType t' (" in function " ++ show p)
    return (Prog (Var' fName fType) args'' e', fType)
tcProg' p = throwError $ "Expected type for function " ++ show (progId p)

tc' :: UExpr -> TCM (TExpr, Type)
tc' (Var (VarId v t)) = do
    envT <- asks $ Map.lookup v . fst
    t' <- case (envT, t) of
        (Just t1, Just t2) -> checkEqOrThrow t1 t2 ("in variable " ++ v)
        (Just t', _) -> return t'
        (_, Just t') -> return t'
        _ -> throwError $ "Unbound variable: " ++ v
    return (Var $ Var' v t', t')
tc' (Lit l) = case l of
    LFloat _ -> return (Lit l, TFloat)
    LBool _  -> return (Lit l, TBool)
    LInt _   -> return (Lit l, TInt)
tc' a@(App e1 e2) = do
    (e1', t1) <- tc' e1
    (e2', t2) <- tc' e2
    ret <- case t1 of
        TFun ta tb -> checkEqOrThrowFn ta t2 (\_ _ -> return tb) (" in application " ++ show a)
        _ -> throwError $ "Expected function, got " ++ show e1 ++ " instead"
    return (App e1' e2', ret)
tc' e@(If c e1 e2) = do
    (c', tcond) <- tc' c
    (e1', t1) <- tc' e1
    (e2', t2) <- tc' e2
    ret <- checkEqOrThrow t1 t2 (" in conditional " ++ show e)
    when (tcond /= TBool) $ throwError $ 
        "Expected " ++ show c ++ " to be Boolean, it's a " ++ show tcond ++ " instead"
    return (If c' e1' e2', ret)
tc' (Cons sName exprs) = do
    (exprs', ts') <- unzip <$> mapM tc' exprs
    structTypes <- asks snd
    fieldDefs <- case Map.lookup sName structTypes of
        Just fds -> return fds
        Nothing -> throwError $ "Undefined struct " ++ sName
    zipWithM_ (\t1 t2 -> checkEqOrThrow t1 t2 $ " in struct " ++ sName) ts' (trdTriple <$> fieldDefs)
    return (Cons sName exprs', TStruct sName)
tc' (FieldGet fName expr) = do
    (expr', t) <- tc' expr
    case t of
        TStruct sName -> structGetter sName expr'
        TTuple _ _ -> tupleIndex >>= \i -> tc' (TupleProj i expr)
        _ -> throwError $ "Field getter: Expected struct, got " ++ show t ++ " instead"
    where
        structGetter :: String -> TExpr -> TCM (TExpr, Type)
        structGetter sName texpr = do
            structTypes <- asks snd
            fieldDefs <- case Map.lookup sName structTypes of
                Just fds -> return fds
                Nothing -> throwError $ "Undefined struct " ++ sName
            (_, _, fieldType) <- case find ((== fName) . fstTriple) fieldDefs of
                Just fieldType -> return fieldType
                Nothing -> throwError $ 
                    "No field `" ++ fName ++ "` on type `" ++ sName ++ "`"
            return (FieldGet fName texpr, fieldType)
        tupleIndex :: TCM Int
        tupleIndex | fName == "x" || fName == "r" = return 0
                   | fName == "y" || fName == "g" = return 1
                   | fName == "z" || fName == "b" = return 2
                   | fName == "w" || fName == "a" = return 3
                   | otherwise = throwError $ "Tuple " ++ show expr ++ " has too few fields"
tc' t@(TupleCons exprs) = do
    (exprs', ts') <- unzip <$> mapM tc' exprs
    let (th:ts) = ts'
    foldM_ (\t1 t2 -> checkEqOrThrow t1 t2 $ " in tuple " ++ show t) th ts
    return (TupleCons exprs', TTuple th $ length exprs)
tc' p@(TupleProj i tup) = do
    (tup', t) <- tc' tup
    case t of
        TTuple t' size -> if i <= size 
            then return (TupleProj i tup', t')
            else throwError $ "Tuple " ++ show tup ++ " has too few fields"
        TStruct sName -> do
            structTypes <- asks snd
            fieldDefs <- case Map.lookup sName structTypes of
                Just fds -> return fds
                Nothing -> throwError $ "Undefined struct " ++ sName
            fieldName <- case fieldDefs !!? i of
                Just (fieldName, _, _) -> return fieldName
                Nothing -> throwError $ "Struct `" ++ sName ++ "` has too few fields"
            tc' (FieldGet fieldName tup)
        _ -> throwError $ "In " ++ show p ++ " expected tuple, got " ++ show tup
tc' (Let (VarId x t) e1 e2) = do
    (e1', t1) <- case t of
        Just t' -> do
            (e1', t1) <- local (first $ Map.insert x t') (tc' e1)
            checkEqOrThrowFn t' t1 (\_ _ -> return (e1', t1)) ""
        Nothing -> tc' e1
    (e2', t2) <- local (first $ Map.insert x t1) (tc' e2)
    return (Let (Var' x t1) e1' e2', t2)
tc' (LetFun prog e) = do
    (prog', tp) <- tcProg' prog
    (e', t') <- local (first $ Map.insert (progId prog) tp) (tc' e)
    return (LetFun prog' e', t')
tc' (BinOp op e1 e2) = do
    (e1', t1) <- tc' e1
    (e2', t2) <- tc' e2
    checkEqOrThrow t1 t2 "" >>= \t -> case t of
        TFloat -> checkNumBinOp op e1' e2' t
        TInt -> checkNumBinOp op e1' e2' t
        TBool -> checkBoolBinOp op e1' e2'
        _ -> throwError $ "Cannot apply binary operation on expressions of type "
                ++ show t
tc' (UnOp op e) = do
    (e', t) <- tc' e
    case (op, t) of
        (OpNeg, TFloat) -> return (UnOp op e', t)
        (OpNeg, TInt) -> return (UnOp op e', t)
        (OpNeg, _) -> throwError $ "Expected number, got " ++ show t ++ " instead"
        (OpNot, TBool)  -> return (UnOp op e', t)
        (OpNot, _) -> throwError $ "Expected Boolean, got " ++ show t ++ " instead"

checkEqOrThrow :: Type -> Type -> Err -> TCM Type
checkEqOrThrow t1 t2 = checkEqOrThrowFn t1 t2 (const . return)

checkEqOrThrowFn :: Type -> Type -> (Type -> Type -> TCM a) -> Err -> TCM a
checkEqOrThrowFn t1 t2 f err = if t1 == t2 
    then f t1 t2 
    else throwError $ show t1 ++ " is not equal to " ++ show t2 ++ err

checkNumBinOp :: BinOp -> TExpr -> TExpr -> Type -> TCM (TExpr, Type)
checkNumBinOp op e1 e2 t = case op of
    OpAdd   -> retT t
    OpMul   -> retT t
    OpSub   -> retT t
    OpDiv   -> retT t
    OpMod   -> retT t
    OpEq    -> retT TBool
    OpNotEq -> retT TBool
    OpLT    -> retT TBool
    OpLTEq  -> retT TBool
    OpGT    -> retT TBool
    OpGTEq  -> retT TBool
    _ -> throwError $ 
            "Expected operation on numbers, got " ++ show op ++ " instead"
    where
        retT t' = return (BinOp op e1 e2, t')

checkBoolBinOp :: BinOp -> TExpr -> TExpr -> TCM (TExpr, Type)
checkBoolBinOp op e1 e2 = case op of
    OpEq -> return (BinOp op e1 e2, TBool)
    OpAnd -> return (BinOp op e1 e2, TBool)
    OpOr -> return (BinOp op e1 e2, TBool)
    _ -> throwError $ 
            "Expected operation on booleans, got " ++ show op ++ " instead"
