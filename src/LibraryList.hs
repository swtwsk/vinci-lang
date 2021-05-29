module LibraryList ( coreLibraryList 
                   , spirLibraryList
                   , spirStructureList ) where

import qualified Data.Map as Map
import Core.AST (Type(..))
import StructDefMap (StructDefMap)
import qualified SPIRV.Types as SType (SpirType(..))

libraryList :: Map.Map String (Type, String)
libraryList = Map.fromList 
    [ ("sin",   (TFun TFloat TFloat, "Sin"))
    , ("cos",   (TFun TFloat TFloat, "Cos"))
    , ("sqrt",  (TFun TFloat TFloat, "Sqrt"))
    , ("abs",   (TFun TFloat TFloat, "FAbs"))
    , ("floor", (TFun TFloat TFloat, "Floor"))
    , ("ceil",  (TFun TFloat TFloat, "Ceil"))
    , ("fract", (TFun TFloat TFloat, "Fract"))
    , ("clamp", (TFun TFloat (TFun TFloat (TFun TFloat TFloat)), "FClamp")) ]

coreLibraryList :: Map.Map String Type
coreLibraryList = Map.map fst libraryList

spirLibraryList :: Map.Map String String
spirLibraryList = Map.map snd libraryList

spirStructureList :: StructDefMap SType.SpirType
spirStructureList = Map.fromList 
    [ ("gl_PerVertex", 
        [ ("gl_Position", Nothing, SType.TVector SType.TFloat 4)
        , ("gl_PointSize", Nothing, SType.TFloat)
        , ("gl_ClipDistance", Nothing, SType.TArray SType.TFloat 1) 
        , ("gl_CullDistance", Nothing, SType.TArray SType.TFloat 1) 
        ])
    ]
