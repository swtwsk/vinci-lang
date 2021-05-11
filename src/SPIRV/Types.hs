module SPIRV.Types where

import Data.List (intercalate)
import SPIRV.SpirOps (SpirStorageClass)

data SpirType = TBool 
              | TInt
              | TUnsignedInt
              | TFloat
              | TVector SpirType Int
              | TStruct String
              | TVoid
              | TPointer SpirStorageClass SpirType
              | TFun SpirType [SpirType]
              deriving (Eq, Ord)

instance Show SpirType where
    show TBool = "bool"
    show TInt = "int"
    show TUnsignedInt = "uint"
    show TFloat = "float"
    show (TVector t size) = "v" ++ show size ++ show t
    show (TStruct sName) = sName
    show TVoid = "void"
    show (TPointer _ t) = "*" ++ show t
    show (TFun ret args) = 
        intercalate " -> " (show <$> args) ++ " -> " ++ show ret
