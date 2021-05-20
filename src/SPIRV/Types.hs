module SPIRV.Types where

import Data.List (intercalate)
import SPIRV.SpirOps (SpirStorageClass)

data SpirType = TBool 
              | TInt
              | TUnsignedInt
              | TFloat
              | TVector SpirType Int
              | TArray SpirType Int
              | TStruct String SpirStructUniform
              | TVoid
              | TPointer SpirStorageClass SpirType
              | TFun SpirType [SpirType]
              deriving (Eq, Ord)

data SpirStructUniform = Uniform | NotUniform deriving (Eq, Ord, Show)

instance Show SpirType where
    show TBool = "bool"
    show TInt = "int"
    show TUnsignedInt = "uint"
    show TFloat = "float"
    show (TVector t size) = "v" ++ show size ++ show t
    show (TArray t size) = show t ++ "[" ++ show size ++ "]"
    show (TStruct sName _) = sName
    show TVoid = "void"
    show (TPointer _ t) = "*" ++ show t
    show (TFun ret args) = 
        intercalate " -> " (show <$> args) ++ " -> " ++ show ret
