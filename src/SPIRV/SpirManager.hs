module SPIRV.SpirManager where

import SPIRV.SpirOps ( SpirOp (OpCapability, OpMemoryModel)
                     , SpirCapability(..) 
                     , SpirAddressingModel(..)
                     , SpirMemoryModel(..) )

data SpirManager = SpirManager 
    { _capabilities     :: [SpirOp]
    , _extensions       :: [SpirOp]
    , _extInstImports   :: [SpirOp]
    , _opMemoryModel    :: SpirOp
    , _opEntryPoints    :: [SpirOp]
    , _executionModes   :: [SpirOp]
    , _debugs           :: ()
    , _annotations      :: [SpirOp]
    , _typeDeclarations :: [SpirOp]
    , _constants        :: [SpirOp]
    , _globalVariables  :: [SpirOp]
    , _functions        :: [SpirOp] }
    deriving (Eq)

spirToLines :: SpirManager -> [String]
spirToLines sm =
    ["; capabilities"] ++ (show <$> _capabilities sm) ++
    ["; extensions"] ++ (show <$> _extensions sm) ++
    ["; extended sets"] ++ (show <$> _extInstImports sm) ++
    ["; memory model"] ++ [show $ _opMemoryModel sm] ++
    ["; entry points"] ++ (show <$> _opEntryPoints sm) ++
    ["; execution modes"] ++ (show <$> _executionModes sm) ++
    ["; annotations"] ++ (show <$> _annotations sm) ++
    ["; type declarations"] ++ (show <$> _typeDeclarations sm) ++
    ["; constants"] ++ (show <$> _constants sm) ++
    ["; global variables"] ++ (show <$> _globalVariables sm) ++
    ["; functions"] ++ (show <$> _functions sm)

defaultManager :: SpirManager
defaultManager = SpirManager
    { _capabilities     = [OpCapability Shader, OpCapability VariablePointers]
    , _extensions       = []
    , _extInstImports   = []
    , _opMemoryModel    = OpMemoryModel Logical GLSL450
    , _opEntryPoints    = []
    , _executionModes   = []
    , _debugs           = ()
    , _annotations      = []
    , _typeDeclarations = []
    , _constants        = []
    , _globalVariables  = []
    , _functions        = [] }
