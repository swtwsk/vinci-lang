module SPIRV.LibraryList where

import qualified Data.Map as Map
import SPIRV.Types

libraryList :: Map.Map String SpirType
libraryList = Map.fromList [ ("sin", TFun TFloat [TFloat])
                           , ("cos", TFun TFloat [TFloat])
                           , ("fabs", TFun TFloat [TFloat])
                           , ("floor", TFun TFloat [TFloat])
                           , ("ceil", TFun TFloat [TFloat])
                           , ("fract", TFun TFloat [TFloat]) ]
