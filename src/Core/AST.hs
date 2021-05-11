{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
module Core.AST where

import Control.Monad.Identity (Identity(Identity))
import Data.List (intercalate)

import Core.Ops (BinOp(..), UnOp(..))

type VarName = String

data VarId f = VarId { _varName :: VarName
                     , _varType :: f Type }

pattern Var' :: VarName -> Type -> VarId Identity
pattern Var' { varN, varT } = VarId { _varName = varN, _varType = Identity varT }

data Binding f = ProgBinding (Prog f) 
               | ConstBinding (VarId f) (Expr f)
               deriving (Eq, Ord)

data Prog f = Prog (VarId f) [VarId f] (Expr f)
            deriving (Eq, Ord)

data Expr f = Var (VarId f)
            | Lit Lit
            | App (Expr f) (Expr f)
            | If (Expr f) (Expr f) (Expr f)
            | Cons String [Expr f]
            | FieldGet String (Expr f)
            | TupleCons [Expr f]
            | TupleProj Int (Expr f)
            | Let (VarId f) (Expr f) (Expr f)
            | LetFun (Prog f) (Expr f)
            | BinOp BinOp (Expr f) (Expr f)
            | UnOp UnOp (Expr f)
            deriving (Eq, Ord)

data Lit = LFloat Double
         | LBool Bool
         | LInt Int
         deriving (Eq, Ord)

data Type = TInt
          | TFloat
          | TBool
          | TFun Type Type
          | TTuple Type Int
          | TStruct String
          | TDummy
        --   | TVar String
          deriving (Eq, Ord)

varId :: VarName -> Type -> VarId Identity
varId n t = VarId n (Identity t)

progId :: Prog f -> VarName
progId (Prog fId _ _) = _varName fId

resType :: Type -> Type
resType (TFun _ t2) = resType t2
resType t = t

-- VARID EQ
instance Eq (VarId f) where
    (VarId name1 _type1) == (VarId name2 _type2) = name1 == name2

instance Ord (VarId f) where
    (VarId name1 _type1) <= (VarId name2 _type2) = name1 <= name2

-- SHOWS
class ShowableFunctor f where
    showF :: (Show a) => f a -> String

instance ShowableFunctor Maybe where
    showF (Just x) = show x
    showF Nothing = ""

instance ShowableFunctor Identity where
    showF (Identity x) = show x

instance (ShowableFunctor f) => Show (VarId f) where
    show (VarId vName vType) = 
        let sType = showF vType in
        if null sType then vName else "(" ++ vName ++ " : " ++ sType ++ ")"

instance (ShowableFunctor f) => Show (Binding f) where
    show (ProgBinding prog) = show prog
    show (ConstBinding var c) = show var ++ " = " ++ show c

instance (ShowableFunctor f) => Show (Prog f) where
    show (Prog progName args expr) = 
        "fn " ++ show progName ++ " " ++ 
        unwords (show <$> args) ++ " = " ++ show expr

instance (ShowableFunctor f) => Show (Expr f) where
    show (Var n) = show n
    show (Lit l) = show l
    show (App v1@(Var _) v2@(Var _)) = show v1 ++ " " ++ show v2
    show (App v1@(Var _) l2@(Lit _)) = show v1 ++ " " ++ show l2
    show (App v1@(Var _) e2) = show v1 ++ " (" ++ show e2 ++ ")"
    show (App e1 v2@(Var _)) = "(" ++ show e1 ++ ") " ++ show v2
    show (App e1 l2@(Lit _)) = "(" ++ show e1 ++ ") " ++ show l2
    show (App e1 e2) = "(" ++ show e1 ++ ")(" ++ show e2 ++ ")"
    show (If cond e1 e2) = "if " ++ show cond ++ " then " ++ show e1 ++ 
        " else " ++ show e2
    show (Cons structName exprs) = 
        structName ++ " { " ++ intercalate ", " (show <$> exprs) ++ " }"
    show (FieldGet field expr) = "(" ++ show expr ++ ")." ++ field
    show (TupleCons exprs) = "(" ++ intercalate ", " (show <$> exprs) ++ ")"
    show (TupleProj i e) = "π" ++ show i ++ " " ++ show e
    show (Let n e1 e2) = "let " ++ show n ++ " = " ++ show e1 ++ " in " ++ show e2
    show (LetFun prog e2) = 
        "let " ++ show prog ++ " in " ++ show e2
    show (BinOp op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
    show (UnOp op e) = show op ++ " " ++ show e

instance Show Lit where
    show lit = case lit of
        LFloat f -> show f
        LBool b  -> show b
        LInt i   -> show i

instance Show Type where
    show t = case t of
        TInt -> "Int"
        TBool -> "Bool"
        TFloat -> "Float"
        TStruct sName -> sName
        -- TVar s -> s
        TFun t1@TFun{} t2@TFun{} -> 
            "(" ++ show t1 ++ ") -> (" ++ show t2 ++ ")"
        TFun t1 t2@TFun{} -> show t1 ++ " -> (" ++ show t2 ++ ")"
        TFun t1@TFun{} t2 -> "(" ++ show t1 ++ ") -> " ++ show t2
        TFun t1 t2 -> show t1 ++ " -> " ++ show t2
        TTuple t' i -> intercalate " × " (show <$> replicate i t')
        TDummy -> "#"
