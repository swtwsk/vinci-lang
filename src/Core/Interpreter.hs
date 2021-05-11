module Core.Interpreter (eval, evalExpr, evalExprEnv, Value(..)) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Bifunctor (first)
import Data.List (intercalate)
import qualified Data.Map as Map

import Core.AST
import Core.Ops

type Err = String
type Env = Map.Map String Value
type EvalM = ReaderT (Env, Map.Map String [String]) (Except Err)

data Value = VFloat Double
           | VBool Bool
           | VInt Int
           | VTuple [Value]
           | VStruct String [Value]
           | VClosure String [String] (Expr Maybe) Env
           deriving (Eq)

-- TODO: FIX INTERPRETER (ACCOUNT FOR STRUCTS!)

eval :: Prog Maybe -> Either Err Value
eval (Prog (VarId fn _) args e) = pure $ VClosure fn (_varName <$> args) e Map.empty

evalExpr :: Expr Maybe -> Either Err Value
evalExpr = evalExprEnv Map.empty

evalExprEnv :: Env -> Expr Maybe -> Either Err Value
evalExprEnv env e = runExcept $ runReaderT (eval' e) (env, Map.empty)

eval' :: Expr Maybe -> EvalM Value
eval' (Var (VarId var _)) = do
    env <- asks fst
    maybe (throwError $ "Unbound variable " ++ var) return $ Map.lookup var env
eval' (Lit l) = case l of
    LFloat f -> return $ VFloat f
    LBool b -> return $ VBool b
    LInt i -> return $ VInt i
eval' (App e1 e2) = do
    e1' <- eval' e1
    e2' <- eval' e2
    apply e1' e2'
eval' (Cons s exprs) = VStruct s <$> mapM eval' exprs
eval' (FieldGet _i _e) = undefined
    -- e' <- eval' e
    -- -- fields <- 
    -- let (VStruct s vals) = e'
    -- return $ vals !! i
eval' (TupleCons exprs) = VTuple <$> mapM eval' exprs
eval' (TupleProj i e) = do
    e' <- eval' e
    let (VTuple vals) = e'
    return $ vals !! i
eval' (If cond e1 e2) = do
    c <- eval' cond
    case c of
        VBool b -> if b then eval' e1 else eval' e2
        _ -> throwError $ show cond ++ " hasn't evaluated to bool"
eval' (Let (VarId n _) e1 e2) = do
    e1' <- eval' e1
    local (first $ Map.insert n e1') (eval' e2)
eval' (LetFun (Prog (VarId f _) args e1) e2) = do
    f' <- asks (VClosure f (_varName <$> args) e1 . fst)
    local (first $ Map.insert f f') (eval' e2)
eval' (BinOp op e1 e2) = do
    e1' <- eval' e1
    e2' <- eval' e2
    binOp e1' e2' op
    where
        binOp :: Value -> Value -> BinOp -> EvalM Value
        binOp (VFloat a) (VFloat b) OpAdd   = return . VFloat $ a + b
        binOp (VFloat a) (VFloat b) OpMul   = return . VFloat $ a * b
        binOp (VFloat a) (VFloat b) OpSub   = return . VFloat $ a - b
        binOp (VFloat _) (VFloat 0.0) OpDiv = throwError "Division by zero"
        binOp (VFloat a) (VFloat b) OpDiv   = return . VFloat $ a / b
        binOp (VFloat a) (VFloat b) OpEq    = return . VBool $ a == b
        binOp (VFloat a) (VFloat b) OpLT    = return . VBool $ a < b
        binOp (VBool a) (VBool b) OpOr  = return . VBool $ a || b
        binOp (VBool a) (VBool b) OpAnd = return . VBool $ a && b
        binOp (VBool a) (VBool b) OpEq  = return . VBool $ a == b
        binOp _ _ _ = throwError "Unexpected error"
eval' (UnOp op e) = do
    e' <- eval' e
    unOp e' op
    where
        unOp :: Value -> UnOp -> EvalM Value
        unOp (VFloat f) OpNeg = return . VFloat $ (-f)
        unOp (VBool b) OpNot  = return . VBool $ not b
        unOp _ _ = throwError "Unexpected error"

apply :: Value -> Value -> EvalM Value
apply f@(VClosure fn [h] e1 cenv) e2 = 
    local (first $ Map.insert fn f . Map.insert h e2 . Map.union cenv) (eval' e1)
apply f@(VClosure fn (h:t) e1 cenv) e2 = do
    newEnv <- asks (Map.insert fn f . Map.insert h e2 . Map.union cenv . fst)
    return $ VClosure "_" t e1 newEnv
-- apply f@(VFixed fn (Lam x e1) cenv) e2 = flip local (eval' e1) $
--     Map.insert fn f . Map.insert x e2 . Map.union cenv
apply e1 e2 = throwError $ "Cannot apply " ++ show e1 ++ " to " ++ show e2

-- SHOW
instance Show Value where
    show (VFloat f)  = show f
    show (VBool b)   = show b
    show (VInt i)    = show i
    show (VTuple vs) = "(" ++ intercalate "," (show <$> vs) ++ ")"
    show (VStruct s vs) = show s ++ " { " ++ intercalate "," (show <$> vs) ++ " }"
    show VClosure {} = "<fun>"
