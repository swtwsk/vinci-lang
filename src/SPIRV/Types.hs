module SPIRV.Types where

import Data.List (intercalate)
import SPIRV.SpirImageConstants
import SPIRV.SpirOps (SpirStorageClass)

data SpirType = TBool 
              | TInt
              | TUnsignedInt
              | TFloat
              | TVector SpirType Int
              | TArray SpirType Int
              | TImage SpirType SpirImageDim SpirImageDepth SpirImageArrayed SpirImageMultisampled SpirImageSampled SpirImageFormat
              | TSampledImage SpirType
              | TVoid
              | TStruct String SpirStructUniform
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
    show (TSampledImage im) = "SampledImage for " ++ show im
    show (TImage t dim depth arrayed ms sampled format) = 
        "Image " ++ show t ++ " " ++ show dim ++ " " ++ show depth ++ " " ++ 
        show arrayed ++ " " ++ show ms ++ " " ++ show sampled ++ " " ++ 
        show format
    show TVoid = "void"
    show (TPointer _ t) = "*" ++ show t
    show (TFun ret args) = 
        intercalate " -> " (show <$> args) ++ " -> " ++ show ret
