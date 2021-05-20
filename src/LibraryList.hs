module LibraryList where

import qualified Data.Map as Map
import Core.AST (Type(..))
import StructDefMap (StructDefMap)
import qualified SPIRV.Types as SType (SpirType(..))

coreLibraryList :: Map.Map String Type
coreLibraryList = Map.fromList [ ("sin", TFun TFloat TFloat)
                               , ("cos", TFun TFloat TFloat)
                               , ("sqrt", TFun TFloat TFloat)
                               , ("fabs", TFun TFloat TFloat)
                               , ("floor", TFun TFloat TFloat)
                               , ("ceil", TFun TFloat TFloat)
                               , ("fract", TFun TFloat TFloat)
                               , ("clamp", TFun TFloat (TFun TFloat (TFun TFloat TFloat))) ]

spirLibraryList :: Map.Map String String
spirLibraryList = Map.fromList [ ("sin", "Sin")
                               , ("cos", "Cos")
                               , ("sqrt", "Sqrt")
                               , ("fabs", "FAbs")
                               , ("floor", "Floor")
                               , ("ceil", "Ceil")
                               , ("fract", "Fract")
                               , ("clamp", "FClamp") ]

spirStructureList :: StructDefMap SType.SpirType
spirStructureList = Map.fromList 
    [ ("gl_PerVertex", [ ("gl_Position", Nothing, SType.TVector SType.TFloat 4)
                       , ("gl_PointSize", Nothing, SType.TFloat)
                       , ("gl_ClipDistance", Nothing, SType.TArray SType.TFloat 1) 
                       , ("gl_CullDistance", Nothing, SType.TArray SType.TFloat 1) 
                       ])
    ]
