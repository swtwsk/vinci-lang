module Core.CoreManager where

import qualified Data.Map as Map

import Core.AST
import StructDefMap (StructDefMap)

type TypeSynonymsMap = Map.Map Type Type

data CoreBindingsManager f = 
    CoreBindingsManager { _bindings         :: [Binding f]
                        , _bindTypeSynonyms :: TypeSynonymsMap
                        , _bindStructDefs   :: StructDefMap Type }
                        deriving Show

data CoreManager f = CoreManager { _progs        :: [Prog f]
                                 , _structDefs   :: StructDefMap Type }
                                 deriving Show

map :: ([Prog f] -> [Prog g]) -> CoreManager f -> CoreManager g
map f (CoreManager progs structDefs) = CoreManager (f progs) structDefs
