module StructDefMap where

import Attribute (Attribute)
import Data.Map (Map)

type StructName  = String
type FieldDef t  = (String, Maybe Attribute, t)
type StructDefMap t = Map StructName [FieldDef t]
