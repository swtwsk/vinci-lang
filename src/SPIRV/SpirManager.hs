module SPIRV.SpirManager where

import SPIRV.SpirOps (SpirOp)

data SpirManager = SpirManager 
    { _capabilities     :: ()
    , _extensions       :: ()
    , _extInstImports   :: ()
    , _opMemoryModel    :: ()
    , _opEntryPoints    :: [SpirOp]
    , _executionModes   :: ()
    , _debugs           :: ()
    , _annotations      :: [SpirOp]
    , _typeDeclarations :: [SpirOp]
    , _constants        :: [SpirOp]
    , _globalVariables  :: [SpirOp]
    , _functions        :: [SpirOp] }
    deriving (Eq)

spirToLines :: SpirManager -> [String]
spirToLines sm =
    ["; entry points"] ++ (show <$> _opEntryPoints sm) ++
    ["; annotations"] ++ (show <$> _annotations sm) ++
    ["; type declarations"] ++ (show <$> _typeDeclarations sm) ++
    ["; constants"] ++ (show <$> _constants sm) ++
    ["; global variables"] ++ (show <$> _globalVariables sm) ++
    ["; functions"] ++ (show <$> _functions sm)

defaultManager :: SpirManager
defaultManager = SpirManager
    { _capabilities     = ()
    , _extensions       = ()
    , _extInstImports   = ()
    , _opMemoryModel    = ()
    , _opEntryPoints    = []
    , _executionModes   = ()
    , _debugs           = ()
    , _annotations      = []
    , _typeDeclarations = []
    , _constants        = []
    , _globalVariables  = []
    , _functions        = [] }
