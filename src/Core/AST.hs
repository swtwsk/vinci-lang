module Core.AST where

import Data.List (intercalate)
type Name = String

data Prog = Prog Name [Name] Expr
          deriving Eq

data Expr = Var Name
          | Lam Name Expr
          | Lit Lit
          | App Expr Expr
          | If Expr Expr Expr
          | Let Name Expr Expr
          | LetRec Name Name Expr Expr
          | TailCall Name Expr
          | BinOp BinOp Expr Expr
          | UnOp UnOp Expr
          deriving Eq

data BinOp = OpAdd 
           | OpMul
           | OpSub
           | OpDiv
           | OpAnd
           | OpOr
           | OpEq
           | OpLT
           deriving Eq

data UnOp = OpNeg 
          | OpNot
          deriving Eq

data Lit = LFloat Double
         | LBool Bool -- later LInt
         deriving Eq

-- SHOWS
instance Show Prog where
    show (Prog progName args expr) = 
        "letfn " ++ progName ++ "(" ++ intercalate ", " args ++ ")" ++ show expr

instance Show Expr where
    show (Var n) = n
    show (Lam n e) = "λ" ++ show n ++ " -> " ++ show e
    show (Lit l) = show l
    show (App e1 e2) = "(" ++ show e1 ++ ")(" ++ show e2 ++ ")"
    show (If cond e1 e2) = "if " ++ show cond ++ " then " ++ show e1 ++ 
        " else " ++ show e2
    show (Let n e1 e2) = "let " ++ show n ++ " = " ++ show e1 ++ " in " ++ show e2
    show (LetRec f x e1 e2) = 
        "letrec " ++ f ++ " " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2
    show (TailCall n e) = "(" ++ show n ++ " " ++ show e ++")"
    show (BinOp op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
    show (UnOp op e) = show op ++ " " ++ show e

instance Show Lit where
    show lit = case lit of
        LFloat f -> show f
        LBool b  -> show b

instance Show BinOp where
    show op = case op of
        OpAdd -> "+"
        OpMul -> "*"
        OpSub -> "-"
        OpDiv -> "/"
        OpAnd -> "and"
        OpOr  -> "or"
        OpEq  -> "=="
        OpLT  -> "<"

instance Show UnOp where
    show op = case op of
        OpNeg -> "-"
        OpNot -> "not"

