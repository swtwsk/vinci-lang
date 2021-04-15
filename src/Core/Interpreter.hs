module Core.Interpreter (eval, evalExpr, evalExprEnv, Value(..)) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.List (intercalate)
import qualified Data.Map as Map

import Core.AST
import Core.Ops

type Err = String
type Env = Map.Map String Value
type EvalM = ReaderT Env (Except Err)

data Value = VFloat Double
           | VBool Bool
           | VTuple [Value]
           | VClosure String [String] Expr Env
           deriving (Eq)

-- TODO: FIX INTERPRETER

eval :: Prog -> Either Err Value
eval (Prog fn args e) = pure $ VClosure fn args e Map.empty

evalExpr :: Expr -> Either Err Value
evalExpr = evalExprEnv Map.empty

evalExprEnv :: Env -> Expr -> Either Err Value
evalExprEnv env e = runExcept $ runReaderT (eval' e) env

eval' :: Expr -> EvalM Value
eval' (Var var) = do
    env <- ask
    maybe (throwError $ "Unbound variable " ++ var) return $ Map.lookup var env
eval' (Lit l) = case l of
    LFloat f -> return $ VFloat f
    LBool b -> return $ VBool b
eval' (App e1 e2) = do
    e1' <- eval' e1
    e2' <- eval' e2
    apply e1' e2'
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
eval' (Let n e1 e2) = do
    e1' <- eval' e1
    local (Map.insert n e1') (eval' e2)
eval' (LetFun (Prog f args e1) e2) = do
    f' <- asks (VClosure f args e1)
    local (Map.insert f f') (eval' e2)
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
    local (Map.insert fn f . Map.insert h e2 . Map.union cenv) (eval' e1)
apply f@(VClosure fn (h:t) e1 cenv) e2 = do
    newEnv <- asks (Map.insert fn f . Map.insert h e2 . Map.union cenv)
    return $ VClosure "_" t e1 newEnv
-- apply f@(VFixed fn (Lam x e1) cenv) e2 = flip local (eval' e1) $
--     Map.insert fn f . Map.insert x e2 . Map.union cenv
apply e1 e2 = throwError $ "Cannot apply " ++ show e1 ++ " to " ++ show e2

-- SHOW
instance Show Value where
    show (VFloat f)  = show f
    show (VBool b)   = show b
    show (VTuple vs) = "(" ++ intercalate "," (show <$> vs) ++ ")"
    show VClosure {} = "<fun>"
