module Core.AST where

import Data.List (intercalate)

import Core.Ops (BinOp(..), UnOp(..))

type Name = String

data ProgType = NonRec | Rec deriving (Eq, Ord)

data Prog = Prog ProgType Name [Name] Expr
          deriving (Eq, Ord)

data Expr = Var Name
          | Lam Name Expr
          | Lit Lit
          | App Expr Expr
          | If Expr Expr Expr
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
    show (Prog _progType progName args expr) = 
        "fn " ++ progName ++ "(" ++ intercalate ", " args ++ ") = " ++ show expr

instance Show Expr where
    show (Var n) = n
    show (Lam n e) = "λ" ++ show n ++ " -> " ++ show e
    show (Lit l) = show l
    show (App e1 e2) = "(" ++ show e1 ++ ")(" ++ show e2 ++ ")"
    show (If cond e1 e2) = "if " ++ show cond ++ " then " ++ show e1 ++ 
        " else " ++ show e2
    show (Let n e1 e2) = "let " ++ show n ++ " = " ++ show e1 ++ " in " ++ show e2
    show (LetFun prog@(Prog progType _ _ _) e2) = slet ++ show prog ++ " in " ++ show e2
        where slet = case progType of { NonRec -> "let" ; Rec -> "letrec" } ++ " "
    show (BinOp op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
    show (UnOp op e) = show op ++ " " ++ show e

instance Show Lit where
    show lit = case lit of
        LFloat f -> show f
        LBool b  -> show b
