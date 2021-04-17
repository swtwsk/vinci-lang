module LibraryList where

import qualified Data.Map as Map
import Core.AST (Type(..))

coreLibraryList :: Map.Map String Type
coreLibraryList = Map.fromList [ ("sin", TFun TFloat TFloat)
                               , ("cos", TFun TFloat TFloat)
                               , ("fabs", TFun TFloat TFloat)
                               , ("floor", TFun TFloat TFloat)
                               , ("ceil", TFun TFloat TFloat)
                               , ("fract", TFun TFloat TFloat) ]

spirLibraryList :: Map.Map String String
spirLibraryList = Map.fromList [ ("sin", "Sin")
                                  , ("cos", "Cos")
                                  , ("fabs", "FAbs")
                                  , ("floor", "Floor")
                                  , ("ceil", "Ceil")
                                  , ("fract", "Fract") ]
