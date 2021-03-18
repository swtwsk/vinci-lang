module Frontend.AST where

import Data.List (intercalate)

newtype Program = Prog [Phrase] deriving (Eq, Ord, Read)

newtype Line = Line Phrase deriving (Eq, Ord, Read)

data Phrase
    = Value LetDef
    | Expression Expr
    | StructDecl StructDef
  deriving (Eq, Ord, Read)

data LetDef = Let [LetBind] | LetRec [LetBind]
  deriving (Eq, Ord, Read)

data LetBind
    = ConstBind LambdaVI Expr
    | ProcBind String [LambdaVI] (Maybe Type) Expr
  deriving (Eq, Ord, Read)

data Expr
    = EId String
    | EInt Integer
    | EFloat Double
    | ETrue
    | EFalse
    | EFieldGet Expr String
    | EApp Expr Expr
    | ETyped Expr Type
    | ENeg Expr
    | ENot Expr
    | EMul Expr Expr
    | EDiv Expr Expr
    | EMod Expr Expr
    | EAdd Expr Expr
    | ESub Expr Expr
    | ELTH Expr Expr
    | ELE Expr Expr
    | EGTH Expr Expr
    | EGE Expr Expr
    | EEQU Expr Expr
    | ENE Expr Expr
    | EAnd Expr Expr
    | EOr Expr Expr
    | ECond Expr Expr Expr
    | ELetIn LetDef Expr
    | ELambda [LambdaVI] Expr
    | ETuple [Expr]
    | ENamedCons String [FieldDef]
    | ECons [FieldDef]
  deriving (Eq, Ord, Read)

data LambdaVI
    = TypedVId LambdaVI Type
    | LambdaVId String
    | WildVId
    | TupleVId [LambdaVI]
  deriving (Eq, Ord, Read)

data FieldDef = FieldDef String Expr
  deriving (Eq, Ord, Read)

data StructDef = SDef String [String] [FieldDecl]
  deriving (Eq, Ord, Read)

data FieldDecl = FieldDecl String Type
  deriving (Eq, Ord, Read)

data Type
    = TInt
    | TFloat
    | TBool
    | TStruct String
    | TPoly String
    | TFun Type Type
  deriving (Eq, Ord, Read)

-- SHOWS
instance Show Program where
    show (Prog phrases) = unlines $ show <$> phrases

instance Show Line where
    show (Line phrase) = show phrase

instance Show Phrase where
    show (Value letdef) = show letdef
    show (Expression expr) = show expr
    show (StructDecl sdef) = show sdef

instance Show LetDef where
    show (Let letbinds) = "let " ++ intercalate " also " (show <$> letbinds)
    show (LetRec letbinds) = "letrec " ++ intercalate " also " (show <$> letbinds)

instance Show LetBind where
    show (ConstBind lambdavi expr) = show lambdavi ++ " = " ++ show expr
    show (ProcBind name args resType expr) = 
        name ++ unwords (show <$> args) ++ maybe "" show resType ++ show expr

instance Show Expr where
    show (EId ident) = ident
    show (EInt i) = show i
    show (EFloat f) = show f
    show ETrue = "True"
    show EFalse = "False"
    show (EFieldGet expr field) = show expr ++ "." ++ field
    show (EApp e1 e2) = "(" ++ show e1 ++ ")(" ++ show e2 ++ ")"
    show (ETyped expr t) = "(" ++ show expr ++ " : " ++ show t ++ ")"
    show (ENeg e) = "-" ++ show e
    show (ENot e) = "not " ++ show e
    show (EMul e1 e2) = show e1 ++ " * " ++ show e2
    show (EDiv e1 e2) = show e1 ++ " / " ++ show e2
    show (EMod e1 e2) = show e1 ++ " % " ++ show e2
    show (EAdd e1 e2) = show e1 ++ " + " ++ show e2
    show (ESub e1 e2) = show e1 ++ " - " ++ show e2
    show (ELTH e1 e2) = show e1 ++ " < " ++ show e2
    show (ELE e1 e2) = show e1 ++ " <= " ++ show e2
    show (EGTH e1 e2) = show e1 ++ " > " ++ show e2
    show (EGE e1 e2) = show e1 ++ " >= " ++ show e2
    show (EEQU e1 e2) = show e1 ++ " == " ++ show e2
    show (ENE e1 e2) = show e1 ++ " != " ++ show e2
    show (EAnd e1 e2) = show e1 ++ " and " ++ show e2
    show (EOr e1 e2) = show e1 ++ " or " ++ show e2
    show (ECond cond e1 e2) = "if " ++ show cond ++ " then " ++ show e1 ++ 
        " else " ++ show e2
    show (ELetIn letdef expr) = show letdef ++ " in " ++ show expr
    show (ELambda lambdavis expr) = "\\" ++ intercalate ", " (show <$> lambdavis) ++ " -> " ++ show expr
    show (ETuple exprs) = "(" ++ intercalate ", " (show <$> exprs) ++ ")"
    show (ENamedCons structName fields) = structName ++ " {\n" ++ intercalate ", " (show <$> fields) ++ "\n}"
    show (ECons fields) = "{\n" ++ intercalate ", " (show <$> fields) ++ "\n}"

instance Show LambdaVI where
    show (TypedVId lambdavi t) = "(" ++ show lambdavi ++ " : " ++ show t ++ ")"
    show (LambdaVId ident) = ident
    show WildVId = "_"
    show (TupleVId lambdavis) = "(" ++ intercalate ", " (show <$> lambdavis) ++ ")"

instance Show FieldDef where
    show (FieldDef fieldName expr) = fieldName ++ " = " ++ show expr

instance Show StructDef where
    show (SDef structName polyIdents fields) = "struct " ++ structName ++ unwords polyIdents ++ " {\n" ++ intercalate ", " (show <$> fields) ++ "\n}"

instance Show FieldDecl where
    show (FieldDecl fieldName t) = fieldName ++ " : " ++ show t

instance Show Type where
    show TInt = "Int"
    show TFloat = "Float"
    show TBool = "Bool"
    show (TStruct structName) = structName
    show (TPoly polyIdent) = polyIdent
    show (TFun t1@TFun {} t2@TFun {}) = "(" ++ show t1 ++ ") -> (" ++ show t2 ++ ")"
    show (TFun t1 t2@TFun {}) = show t1 ++ " -> (" ++ show t2 ++ ")"
    show (TFun t1@TFun {} t2) = "(" ++ show t1 ++ ") -> " ++ show t2
    show (TFun t1 t2) = show t1 ++ " -> " ++ show t2
