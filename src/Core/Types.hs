module Core.Types where

import Data.List (intercalate)
import Data.Map

data Type = TInt
          | TFloat
          | TBool
          | TFun Type Type
          | TTuple Type Int
          | TMatrix Type Int
          | TStruct String
          | TSampler Int
          | TVar Tyvar
          | TDummy
          deriving (Eq, Ord)

newtype Tyvar = Tyvar String deriving (Eq, Ord, Show)

data Class = ClassEq
           | ClassOrd 
           | ClassNum 
           | ClassFloating 
           | ClassVector
           | ClassMatrix
           | ClassVectorOrMatrix
           deriving (Eq, Ord)

data Pred = IsIn Class Type
          | SameSize Type Type
          deriving Eq

data Qual t = [Pred] :=> t
            deriving Show

data Scheme = Scheme [Tyvar] (Qual Type) 
            deriving Show

type ClassEnv = Map Class [Type]

toScheme :: Type -> Scheme
toScheme t = Scheme [] ([] :=> t)

fromScheme :: Scheme -> Type
fromScheme (Scheme _ (_ :=> t)) = t

classEnv :: ClassEnv
classEnv = fromList [ (ClassEq,  concat [ t:vectors t | t <- [TBool, TInt, TFloat] ])
                    , (ClassOrd, concat [ t:vectors t | t <- [TInt, TFloat] ])
                    , (ClassNum, concat [ t:vectors t | t <- [TInt, TFloat] ])
                    , (ClassFloating, TFloat:vectors TFloat)
                    , (ClassVector, vectors TFloat)
                    , (ClassMatrix, matrices TFloat)
                    , (ClassVectorOrMatrix, vectors TFloat ++ matrices TFloat)
                    ]

vectors :: Type -> [Type]
vectors t = [ TTuple t i | i <- [2..4] ]

matrices :: Type -> [Type]
matrices t = [ TMatrix t i | i <- [2..4] ]

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
        TMatrix t' i -> "Mat" ++ show i ++ " of " ++ show t'
        TSampler i -> "Sampler" ++ show i ++ "D"
        TDummy -> "#"

instance Show Pred where
    show (IsIn c t) = show t ++ " in " ++ show c
    show (SameSize t1 t2) = show t1 ++ " has the same size as " ++ show t2

instance Show Class where
    show ClassEq             = "Eq"
    show ClassOrd            = "Ord"
    show ClassNum            = "Num"
    show ClassFloating       = "Floating"
    show ClassVector         = "Vector"
    show ClassMatrix         = "Matrix"
    show ClassVectorOrMatrix = "VectorOrMatrix"
