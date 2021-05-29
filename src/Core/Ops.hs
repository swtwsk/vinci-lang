module Core.Ops (BinOp(..), UnOp(..)) where

data BinOp = OpAdd 
           | OpMul
           | OpSub
           | OpDiv
           | OpMod
           | OpAnd
           | OpOr
           | OpEq
           | OpNotEq
           | OpLT
           | OpLTEq
           | OpGT
           | OpGTEq
           deriving (Eq, Ord)

data UnOp = OpNeg 
          | OpNot
          deriving (Eq, Ord)

instance Show BinOp where
    show op = case op of
        OpAdd   -> "+"
        OpMul   -> "*"
        OpSub   -> "-"
        OpDiv   -> "/"
        OpMod   -> "%"
        OpAnd   -> "and"
        OpOr    -> "or"
        OpEq    -> "=="
        OpNotEq -> "!="
        OpLT    -> "<"
        OpLTEq  -> "<="
        OpGT    -> ">"
        OpGTEq  -> ">="

instance Show UnOp where
    show op = case op of
        OpNeg -> "-"
        OpNot -> "not"
