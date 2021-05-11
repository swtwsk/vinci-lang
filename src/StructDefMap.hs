module StructDefMap where

import Data.Map (Map)

type StructName  = String
type FieldDef t  = (String, t)
type StructDefMap t = Map StructName [FieldDef t]
