module Core.Types where

import Data.List (intercalate)
import Data.Map

data Type = TInt
          | TFloat
          | TBool
          | TFun Type Type
          | TTuple Type Int
          | TStruct String
          | TVar Tyvar
          | TDummy
          deriving (Eq, Ord)

newtype Tyvar = Tyvar String deriving (Eq, Ord, Show)

data Class = ClassEq | ClassOrd | ClassNum deriving (Eq, Ord)

data Pred = IsIn Class Type 
          deriving Eq

data Qual t = [Pred] :=> t
            deriving Show

data Scheme = Scheme [Tyvar] (Qual Type) 
            deriving Show

type ClassEnv = Map Class ([Class], [Type])

toScheme :: Type -> Scheme
toScheme t = Scheme [] ([] :=> t)

fromScheme :: Scheme -> Type
fromScheme (Scheme _ (_ :=> t)) = t

classEnv :: ClassEnv
classEnv = fromList [ (ClassEq,  ([], [TBool, TInt, TFloat]))
                    , (ClassOrd, ([ClassEq], [TInt, TFloat]))
                    , (ClassNum, ([ClassEq, ClassOrd], [TInt, TFloat]))
                    ]

-- SHOWS

instance Show Type where
    show t = case t of
        TInt -> "Int"
        TBool -> "Bool"
        TFloat -> "Float"
        TStruct sName -> sName
        TVar (Tyvar s) -> s
        TFun t1@TFun{} t2@TFun{} -> 
            "(" ++ show t1 ++ ") -> (" ++ show t2 ++ ")"
        TFun t1 t2@TFun{} -> show t1 ++ " -> (" ++ show t2 ++ ")"
        TFun t1@TFun{} t2 -> "(" ++ show t1 ++ ") -> " ++ show t2
        TFun t1 t2 -> show t1 ++ " -> " ++ show t2
        TTuple t' i -> "(" ++ intercalate ", " (show <$> replicate i t') ++ ")"
        TDummy -> "#"

instance Show Pred where
    show (IsIn c t) = show t ++ " in " ++ show c

instance Show Class where
    show ClassEq  = "Eq"
    show ClassOrd = "Ord"
    show ClassNum = "Num"
