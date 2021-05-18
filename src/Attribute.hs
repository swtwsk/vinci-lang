module Attribute (Attribute(..)) where

data Attribute = Location Int 
               | Binding Int
               deriving (Eq, Ord, Show, Read)
