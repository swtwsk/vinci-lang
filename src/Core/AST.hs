module Core.AST where

import Data.List (intercalate)

import Core.Ops (BinOp(..), UnOp(..))

type Name = String

data Prog = Prog Name [Name] Expr
          deriving (Eq, Ord)

data Expr = Var Name
          | Lit Lit
          | App Expr Expr
          | If Expr Expr Expr
          | TupleCons [Expr]
          | TupleProj Int Expr
          | Let Name Expr Expr
          | LetFun Prog Expr
          | BinOp BinOp Expr Expr
          | UnOp UnOp Expr
          deriving (Eq, Ord)

data Lit = LFloat Double
         | LBool Bool -- later LInt
         deriving (Eq, Ord)

-- SHOWS
instance Show Prog where
    show (Prog progName args expr) = 
        "fn " ++ progName ++ " " ++ unwords args ++ " = " ++ show expr

instance Show Expr where
    show (Var n) = n
    show (Lit l) = show l
    show (App e1 e2) = "(" ++ show e1 ++ ")(" ++ show e2 ++ ")"
    show (If cond e1 e2) = "if " ++ show cond ++ " then " ++ show e1 ++ 
        " else " ++ show e2
    show (TupleCons exprs) = "(" ++ intercalate "," (show <$> exprs) ++ ")"
    show (TupleProj i e) = "π" ++ show i ++ " " ++ show e
    show (Let n e1 e2) = "let " ++ n ++ " = " ++ show e1 ++ " in " ++ show e2
    show (LetFun prog e2) = 
        "let " ++ show prog ++ " in " ++ show e2
    show (BinOp op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
    show (UnOp op e) = show op ++ " " ++ show e

instance Show Lit where
    show lit = case lit of
        LFloat f -> show f
        LBool b  -> show b
