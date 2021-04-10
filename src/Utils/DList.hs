{-# LANGUAGE FlexibleContexts #-}
module Utils.DList (output) where

import Control.Monad.Writer (MonadWriter, tell)
import Data.DList (DList, singleton)

output :: (Monad m, MonadWriter (DList a) m) => a -> m ()
output = tell . singleton
