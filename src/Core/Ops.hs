module Core.Ops (BinOp(..), UnOp(..)) where

data BinOp = OpAdd 
           | OpMul
           | OpSub
           | OpDiv
           | OpMod
           | OpAnd
           | OpOr
           | OpEq
           | OpLT
           deriving Eq

data UnOp = OpNeg 
          | OpNot
          deriving Eq

instance Show BinOp where
    show op = case op of
        OpAdd -> "+"
        OpMul -> "*"
        OpSub -> "-"
        OpDiv -> "/"
        OpMod -> "%"
        OpAnd -> "and"
        OpOr  -> "or"
        OpEq  -> "=="
        OpLT  -> "<"

instance Show UnOp where
    show op = case op of
        OpNeg -> "-"
        OpNot -> "not"
