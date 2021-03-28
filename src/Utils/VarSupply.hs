-- VarSupplyT is heavily based on a standard implementation of SupplyT

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.VarSupply (
    MonadVarSupply(..),
    VarSupplyT,
    VarSupply,
    evalVarSupplyT,
    evalVarSupply,
    runVarSupplyT,
    runVarSupply
) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Zip

newtype VarSupplyT s m a = VarSupplyT (StateT [s] m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

newtype VarSupply s a = VarSupply (VarSupplyT s Identity a)
    deriving (Functor, Applicative, Monad, MonadVarSupply s)

class Monad m => MonadVarSupply s m | m -> s where
    nextVar   :: m s
    buildVar  :: (s -> s) -> m s
    getSupply :: m [s]
    putSupply :: [s] -> m ()

instance Monad m => MonadVarSupply s (VarSupplyT s m) where
    nextVar = VarSupplyT $ do
        (x, xs) <- gets fromInfiniteList
        put xs
        return x
    buildVar f = fmap f nextVar
    getSupply = VarSupplyT get
    putSupply newSupp = VarSupplyT $ put newSupp

instance MonadVarSupply s m => MonadVarSupply s (ReaderT r m) where
    nextVar   = lift nextVar
    buildVar  = lift . buildVar
    getSupply = lift getSupply
    putSupply = lift . putSupply

instance MonadVarSupply s m => MonadVarSupply s (StateT s m) where
    nextVar   = lift nextVar
    buildVar  = lift . buildVar
    getSupply = lift getSupply
    putSupply = lift . putSupply

instance (MonadVarSupply s m, Monoid w) => MonadVarSupply s (WriterT w m) where
    nextVar   = lift nextVar
    buildVar  = lift . buildVar
    getSupply = lift getSupply
    putSupply = lift . putSupply

instance (MonadVarSupply s m) => MonadVarSupply s (ExceptT e m) where
    nextVar   = lift nextVar
    buildVar  = lift . buildVar
    getSupply = lift getSupply
    putSupply = lift . putSupply

instance MonadZip (VarSupply s) where
  mzipWith = liftM2

instance (MonadVarSupply s m) => MonadZip (VarSupplyT s m) where
  mzipWith = liftM2

evalVarSupplyT :: Monad m => VarSupplyT s m a -> [s] -> m a
evalVarSupplyT (VarSupplyT vs) = evalStateT vs

evalVarSupply :: VarSupply s a -> [s] -> a
evalVarSupply (VarSupply s) = runIdentity . evalVarSupplyT s

runVarSupplyT :: Monad m => VarSupplyT s m a -> [s] -> m (a, [s])
runVarSupplyT (VarSupplyT vs) = runStateT vs

runVarSupply :: VarSupply s a -> [s] -> (a, [s])
runVarSupply (VarSupply s) = runIdentity . runVarSupplyT s

-- new GHC version fix, courtesy of haskell-chart repository
-- https://github.com/timbod7/haskell-chart/pull/197
fromInfiniteList :: [a] -> (a, [a])
fromInfiniteList []     = error "VarSupply: empty list"
fromInfiniteList (x:xs) = (x, xs)
