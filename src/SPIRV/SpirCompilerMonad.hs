{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SPIRV.SpirCompilerMonad 
    ( SpirCompiler 
    -- , CompilerException
    , StateEnv(..)
    , TypeIdsMap
    , runCompiler
    , execCompiler
    , evalCompiler
    , initialStateEnv
    , nextVar
    , buildVar
    , output
    , insertRenamed
    , getRenamedVar
    , module Control.Monad.State.Class ) where

-- import Control.Monad.Except
import Control.Monad.State.Class (gets, modify, modify')
import Control.Monad.State (StateT(StateT), execStateT, runStateT, MonadState)
import Control.Monad.Writer
import Data.DList (DList)
import qualified Data.Map as Map

import StructDefMap (StructDefMap)
import SPIRV.SpirOps
import SPIRV.Types
import Utils.DList (output)
import Utils.VarSupply (fromInfiniteList)

-- | A monad containing an updatable state of type @StateEnv@
-- and output of type @SpirList@.
newtype SpirCompiler a = SpirCompiler 
    { unCompiler :: StateT StateEnv (Writer SpirList) a } 
    deriving ( Applicative
             , Functor
             , Monad
             , MonadState StateEnv
             , MonadWriter SpirList )

-- type CompilerException = ()
data StateEnv = StateEnv { _structDefs    :: StructDefMap SpirType
                         , _typeIds       :: TypeIdsMap
                         , _renames       :: Map.Map String String
                         , _phiVars       :: Map.Map String SpirType
                         , _varSupply     :: [String]
                         , _entryPoints   :: [SpirOp]
                         , _globalVars    :: [SpirOp]
                         , _stAnnotations :: [SpirOp] }
type SpirList = DList SpirOp

type TypeIdsMap = Map.Map SpirType SpirId

runCompiler :: StateEnv -> SpirCompiler a -> (a, StateEnv, SpirList)
runCompiler stateEnv compiler = (result, finalState, writerOutput)
    where
        ((result, finalState), writerOutput) = 
            runWriter (runStateT (unCompiler compiler) stateEnv)

execCompiler :: StateEnv
             -> SpirCompiler a 
             -> (StateEnv, SpirList)
execCompiler stateEnv compiler = 
    runWriter (execStateT (unCompiler compiler) stateEnv)

evalCompiler :: StateEnv -> SpirCompiler a -> (a, StateEnv)
evalCompiler = (tripleToPair .) . runCompiler
    where
        tripleToPair (a, b, _) = (a, b)

initialStateEnv :: StructDefMap SpirType -> StateEnv
initialStateEnv structDefs = StateEnv
    { _structDefs    = structDefs
    , _typeIds       = Map.empty
    , _renames       = Map.empty
    , _phiVars       = Map.empty
    , _varSupply     = [show i | i <- [(2 :: Int)..]]
    , _entryPoints   = []
    , _globalVars    = []
    , _stAnnotations = [] }

nextVar :: SpirCompiler String
nextVar = do
    (x, xs) <- gets (fromInfiniteList . _varSupply)
    modify $ \s -> s { _varSupply = xs }
    return x

buildVar :: (String -> String) -> SpirCompiler String
buildVar f = fmap f nextVar

insertRenamed :: String -> String -> SpirCompiler ()
insertRenamed var renamedVar = do
    renames <- gets _renames
    let renames' = Map.insert var renamedVar renames
    modify $ \st -> st { _renames = renames' }

getRenamedVar :: String -> SpirCompiler String
getRenamedVar var = do
    renames <- gets _renames
    swap1_3 maybe return (Map.lookup var renames) $ do
        var' <- nextVar
        insertRenamed var var'
        return var'

-- courtesy of https://stackoverflow.com/a/12131896
swap1_3 :: (a -> b -> c -> d) -> (b -> c -> a -> d)
swap1_3 = (flip .) . flip
