{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | Handle conversion of SSA to SPIR-V code
module SPIRV.SSAtoSPIR (ssaToSpir) where

import Control.Monad
import Data.Bifunctor (first, second)
import Data.DList (toList)
import Data.List (elemIndex)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)

import qualified Attribute
import Core.Ops
import LibraryList (floatToInt, intToFloat, spirLibraryList, spirStructureList)
import ManglingPrefixes (ssaToSpirLabelPrefix)
import StructDefMap (FieldDef, StructDefMap)
import SSA.AST
import SPIRV.DecorateOffsets (decorateOffsets)
import SPIRV.SpirCompilerMonad
import qualified SPIRV.SpirManager as SpirManager (SpirManager(..), defaultManager)
import SPIRV.SpirOps
import SPIRV.Types
import Utils.Tuple

type ReturnCallback = SExpr -> SpirCompiler () -> SpirCompiler ()

-- -----------------------------------------------------------------------------
-- | Top-level of the SPIR-V code generator
--
ssaToSpir :: ([SFnDef], StructDefMap SpirType) -> SpirManager.SpirManager
ssaToSpir (fnDefs, structDefs) = SpirManager.defaultManager
    { SpirManager._extInstImports   = snd <$> Map.toList (_extInstImports finalSt)
    , SpirManager._opEntryPoints    = _entryPoints finalSt
    , SpirManager._executionModes   = _executionModes finalSt
    , SpirManager._annotations      = 
        _annotations finalSt ++ decorateOffsets finalStructDef finalTypeIds
    , SpirManager._typeDeclarations = typeDeclarations
    , SpirManager._constants        = fst . snd <$> Map.toList (_constants finalSt)
    , SpirManager._globalVariables  = _globalVariables finalSt
    , SpirManager._functions        = hoistVariables (toList ops)
    }
    where
        (frag, vert, rest) = extractMains fnDefs
        initialState = initialStateEnv $ Map.union structDefs spirStructureList
        (typeDeclarations, finalSt, ops) = runCompiler initialState $ do
            mapM_ fragMainToSpir frag
            mapM_ vertMainToSpir vert
            mapM_ fnDefToSpir rest
            typesToOps
        finalStructDef = _structDefs finalSt
        finalTypeIds   = _typeIds finalSt

-- | Search for shader entry points
extractMains :: [SFnDef] -> (Maybe SFnDef, Maybe SFnDef, [SFnDef])
extractMains = flip foldr (Nothing, Nothing, []) $
    \f@(SFnDef fName _ _ _ _) (frag, vert, rest) -> case fName of
        "frag" -> (Just f, vert, rest)
        "vert" -> (frag, Just f, rest)
        _      -> (frag, vert, f:rest)

-- | Hoist function-scoped variables to the beginning of the first block
-- - required from SPIR-V specification
hoistVariables :: [SpirOp] -> [SpirOp]
hoistVariables = concat . (uncurry hoistVariables' . extractVariables <$>)
                        . (fst . splitToFns)
    where
        hoistVariables' :: [SpirOp] -> [SpirOp] -> [SpirOp]
        hoistVariables' vars (l@(OpLabel _):t) = l:vars ++ t
        hoistVariables' vars (h:t) = h:hoistVariables' vars t
        hoistVariables' _ [] = []

        extractVariables :: [SpirOp] -> ([SpirOp], [SpirOp])
        extractVariables (v@OpVariable {}:t) = first (v:) $ extractVariables t
        extractVariables (h:t) = second (h:) $ extractVariables t
        extractVariables [] = ([], [])

        splitToFns :: [SpirOp] -> ([[SpirOp]], [SpirOp])
        splitToFns (f@OpFunction {}:t) =
            let (fns, currFn) = splitToFns t in ((f:currFn):fns, [])
        splitToFns (OpFunctionEnd:t) =
            let (fns, _currFn) = splitToFns t in (fns, [OpFunctionEnd])
        splitToFns (h:t) = second (h:) $ splitToFns t
        splitToFns [] = ([], [])

-- -----------------------------------------------------------------------------
-- * Function code generation
--

-- | Generate code for function that's not an entry point
fnDefToSpir :: SFnDef -> SpirCompiler ()
fnDefToSpir (SFnDef fName rt fArgs block labelled) =
    fnDefToSpir' fName fName rt fArgs block labelled (return $ \_ ret -> ret)

-- | Generate code for vertex shader entry point function
vertMainToSpir :: SFnDef -> SpirCompiler ()
vertMainToSpir (SFnDef fName rt fArgs block labelled) =
    fnDefToSpir' fName "main" TVoid [] block labelled preBlockProcessor
    where
        retCallback :: [Var] -> ReturnCallback
        retCallback outCtrVars = \(SVar var) _ -> do
            let getters = map (`SStructGet` var) [0..length outCtrVars - 1]
            zipWithM_ ((stmtToSpir (\_ x -> x) False .) . SAssign) outCtrVars getters
            output OpReturn

        preBlockProcessor :: SpirCompiler ReturnCallback
        preBlockProcessor = do
            let (SArg uniVar:SArg insVar:_) = fArgs
                (Var _uniName uniT@(TStruct uniStruct _)) = uniVar
                (Var _insName insT@(TStruct insStruct _)) = insVar
                (TStruct outsStruct _) = rt

            insArgs <- gets $ (Map.! insStruct) . _structDefs
            insCtrVars <- mapM processIn insArgs
            stmtToSpir (\_ x -> x) True $
                        SAssign insVar (SStructCtr insT insCtrVars)

            uniArgs <- gets $ (Map.! uniStruct) . _structDefs
            uniCtrVars <- mapM entryPointArgumentToUniform uniArgs
            stmtToSpir (\_ x -> x) True $
                        SAssign uniVar (SStructCtr uniT uniCtrVars)

            outArgs <- gets $ (Map.! outsStruct) . _structDefs
            let outs = removePredefined outArgs
            predefinedStructVar <- processPredefinedVertexOutputs
            outCtrVars <- mapM (mapPredefined predefinedStructVar) outArgs

            entryPointVarIds <- mapM ((SpirId <$>) . getRenamedVar) $
                        (fstTriple <$> insArgs) ++ (fstTriple <$> outs) ++ [predefinedStructVar]
            let entryPoint = OpEntryPoint Vertex (SpirId "main") "main" entryPointVarIds
            modify $ \st -> st { _entryPoints = entryPoint:_entryPoints st }
            return (retCallback outCtrVars)

        processIn  = entryPointArgumentToGlobal StorInput
        processOut = entryPointArgumentToGlobal StorOutput

        removePredefined = filter (flip notElem [ "gl_Position"
                                                , "gl_PointSize" 
                                                , "gl_ClipDistance"
                                                , "gl_CullDistance" ] . fstTriple)
        mapPredefined var = \field@(fieldName, _, _) -> case fieldName of
                "gl_Position"     -> processPredefinedVertexOutput var field
                "gl_PointSize"    -> processPredefinedVertexOutput var field
                "gl_ClipDistance" -> processPredefinedVertexOutput var field
                "gl_CullDistance" -> processPredefinedVertexOutput var field
                _                 -> processOut field

-- | Generate code for fragment shader entry point function
fragMainToSpir :: SFnDef -> SpirCompiler ()
fragMainToSpir (SFnDef fName rt fArgs block labelled) =
    fnDefToSpir' fName "main" TVoid [] block labelled preBlockProcessor
    where
        retCallback :: [Var] -> ReturnCallback
        retCallback outCtrVars = \(SVar var) _ -> do
            let getters = map (`SStructGet` var) [0..length outCtrVars - 1]
            zipWithM_ ((stmtToSpir (\_ x -> x) False .) . SAssign) outCtrVars getters
            output OpReturn

        preBlockProcessor :: SpirCompiler ReturnCallback
        preBlockProcessor = do
            let (SArg uniVar:SArg insVar:_) = fArgs
                (Var _uniName uniT@(TStruct uniStruct _)) = uniVar
                (Var _insName insT@(TStruct insStruct _)) = insVar
                (TStruct outsStruct _) = rt

            insArgs <- gets $ (Map.! insStruct) . _structDefs
            insCtrVars <- mapM processIn insArgs
            stmtToSpir (\_ x -> x) True $
                        SAssign insVar (SStructCtr insT insCtrVars)

            uniArgs <- gets $ (Map.! uniStruct) . _structDefs
            uniCtrVars <- mapM entryPointArgumentToUniform uniArgs
            stmtToSpir (\_ x -> x) True $
                        SAssign uniVar (SStructCtr uniT uniCtrVars)

            outArgs <- gets $ (Map.! outsStruct) . _structDefs
            outCtrVars <- mapM processOut outArgs

            entryPointVarIds <- mapM ((SpirId <$>) . getRenamedVar) $
                        (fstTriple <$> insArgs) ++ (fstTriple <$> outArgs)
            let mainId = SpirId "main"
                entryPoint = OpEntryPoint Fragment mainId "main" entryPointVarIds
                executionMode = OpExecutionMode mainId OriginUpperLeft
            modify $ \st -> 
                st { _entryPoints    = entryPoint:_entryPoints st
                   , _executionModes = executionMode:_executionModes st }
            return $ retCallback outCtrVars

        processIn  = entryPointArgumentToGlobal StorInput
        processOut = entryPointArgumentToGlobal StorOutput

-- -----------------------------------------------------------------------------
-- * Function code generation helper functions
--

-- | Helper for generating code for function
fnDefToSpir' :: String                       -- ^ SSA function name
             -> String                       -- ^ SPIR-V compiled function name
             -> SpirType                     -- ^ function return type
             -> [SArg]                       -- ^ list of arguments
             -> SBlock                       -- ^ entry block
             -> [SLabelledBlock]             -- ^ list of the rest of the blocks
             -> SpirCompiler ReturnCallback  -- ^ function to be called before 
                                             -- generating code for blocks
             -> SpirCompiler ()
fnDefToSpir' funName compiledFunName returnType arguments block labelledBlocks preBlockProcessor = do
    retType  <- getTypeId returnType
    let ats  = (\(SArg a) -> TPointer StorFunction $ _varType a) <$> arguments
    funType  <- getTypeId $ TFun returnType ats
    output $ OpFunction (SpirId compiledFunName) retType FCNone funType

    renamedArgs <- forM arguments $ \(SArg (Var arg _)) ->
                        (arg, ) <$> buildVar ((arg ++ "_")++)
    zipWithM_ processArgs renamedArgs ats
    renamedLabels <- mapM (\(SLabelled (SLabel l) _ _) -> (toLabel l, ) <$> nextVar) labelledBlocks
    let renameMap = Map.fromList (renamedArgs ++ renamedLabels)
    label <- nextVar
    output $ OpLabel (SpirId label)
    let renameMap' = Map.insert (toLabel $ funName ++ "_init") label renameMap
    modify $ \st -> st { _renames  = renameMap' }

    retCallback <- preBlockProcessor

    blockToSpir retCallback block
    forM_ labelledBlocks (labelledToSpir retCallback)

    output OpFunctionEnd
    where
        processArgs :: (a, String) -> SpirType -> SpirCompiler ()
        processArgs (_, rArg) t = do
            typeId <- getTypeId t
            let argId = SpirId rArg
            output $ OpFunctionParameter argId typeId

-- | Convert shader entry point argument to a global variable
entryPointArgumentToGlobal :: SpirStorageClass  -- ^ global var storage class
                           -> FieldDef SpirType -- ^ field to process
                           -> SpirCompiler Var  -- ^ global variable
entryPointArgumentToGlobal sc (fieldName, Just (Attribute.Location l), fType) =
    entryPointArgumentToGlobal' sc annotationFn fieldName fType
    where
        annotationFn = \fieldId -> [OpDecorate (SpirId fieldId) Location [l]]
entryPointArgumentToGlobal _ _ = undefined

-- | Convert shader entry point argument to a uniform global variable
entryPointArgumentToUniform :: FieldDef SpirType -> SpirCompiler Var
entryPointArgumentToUniform ( fieldName
                            , Just (Attribute.Binding b)
                            , fType@(TStruct structName _) ) = do
    (_, uniformStructType) <- structToUniformStruct structName
    uniformStructTypeId    <- getTypeId uniformStructType
    let annotation = OpDecorate uniformStructTypeId Block []
    modify $ \st -> st { _annotations = annotation:_annotations st }

    fieldRenamed   <- getRenamedVar fieldName
    uniformVarType <- getTypeId (TPointer StorUniform uniformStructType)
    let variable = OpVariable (SpirId fieldRenamed) uniformVarType StorUniform
        annotations = getUniformAnnotations b fieldRenamed
    modify $ \st -> st { _globalVariables = variable:_globalVariables st
                       , _annotations     = annotations ++ _annotations st }

    newId <- extractUniformToVariable fieldName structName
    tmpVar <- buildVar ((fieldName ++) . ('_':))
    varType <- getTypeId (TPointer StorFunction fType)

    output $ OpVariable (SpirId tmpVar) varType StorFunction
    output $ OpStore (SpirId tmpVar) newId

    insertRenamed tmpVar tmpVar
    return $ Var tmpVar fType
-- TODO: Following should be used just for sampler/image
-- entryPointArgumentToUniform (fieldName, Just (Attribute.Binding b), fType) =
--     entryPointArgumentToGlobal' StorUniformConstant (getUniformAnnotations b) fieldName fType
entryPointArgumentToUniform _ = undefined

-- | List annotations (decorations) for uniform bindings
getUniformAnnotations :: Int -> String -> [SpirOp]
getUniformAnnotations b fieldId = [ OpDecorate (SpirId fieldId) Binding [b]
                                  , OpDecorate (SpirId fieldId) DescriptorSet [0] ]

-- | Generate code copying global variable to a local one
entryPointArgumentToGlobal' :: SpirStorageClass
                            -> (String -> [SpirOp])
                            -> String
                            -> SpirType
                            -> SpirCompiler Var
entryPointArgumentToGlobal' sc annotationsFn fieldName fType = do
    fieldRenamed <- getRenamedVar fieldName
    varType <- getTypeId (TPointer sc fType)
    let variable    = OpVariable (SpirId fieldRenamed) varType sc
        annotations = annotationsFn fieldRenamed
    modify $ \st -> st { _globalVariables = variable:_globalVariables st
                       , _annotations     = annotations ++ _annotations st }
    return $ Var fieldName fType

predefinedVertexOutputId :: String
predefinedVertexOutputId = "_vertex"

processPredefinedVertexOutputs :: SpirCompiler String
processPredefinedVertexOutputs = do
    let structType = TStruct "gl_PerVertex" NotUniform
    outputStructType <- getTypeId structType
    outputVarType    <- getTypeId (TPointer StorOutput structType)
    let variable    = OpVariable (SpirId predefinedVertexOutputId) outputVarType StorOutput
        annotations = [ OpDecorate outputStructType Block []
                      , OpMemberDecorate outputStructType 0 BuiltIn [Right Position]
                      , OpMemberDecorate outputStructType 1 BuiltIn [Right PointSize]
                      , OpMemberDecorate outputStructType 2 BuiltIn [Right ClipDistance]
                      , OpMemberDecorate outputStructType 3 BuiltIn [Right CullDistance] ]
    modify $ \st -> st { _globalVariables = variable:_globalVariables st
                       , _annotations     = annotations ++ _annotations st }
    insertRenamed predefinedVertexOutputId predefinedVertexOutputId
    return predefinedVertexOutputId

processPredefinedVertexOutput :: String -> FieldDef SpirType -> SpirCompiler Var
processPredefinedVertexOutput var (fieldName, Nothing, fType) = do
    let structName = "gl_PerVertex"
    fieldList <- gets $ (Map.! structName) . _structDefs
    let i = fromMaybe undefined (elemIndex fieldName (fstTriple <$> fieldList))
    (iVar, _) <- getConstId (CSignedInt i)

    resVar <- nextVar
    var' <- SpirId <$> getRenamedVar var
    ptrVarType <- getTypeId (TPointer StorOutput fType)

    output $ OpAccessChain (SpirId resVar) ptrVarType var' iVar
    insertRenamed resVar resVar
    return (Var resVar fType)
processPredefinedVertexOutput _ _ = undefined

-- | Recursively walk through the uniform struct and copy its content to 
-- a function-scoped variable 
extractUniformToVariable :: String -> String -> SpirCompiler SpirId
extractUniformToVariable structVar structName = do
    (uniformFields, uniformStructType) <- structToUniformStruct structName
    let uniformFields' = zip uniformFields [0..]

    ctrVars <- forM uniformFields' $ \((fieldName, _attr, fType), i) -> case fType of
        TStruct innerStructName _ -> do
            tmpVar <- buildVar ((fieldName ++) . ('_':))
            (SpirId tmpId, _) <- structGetToSpir i structVar uniformStructType StorUniform
            insertRenamed tmpVar tmpId
            extractUniformToVariable tmpVar innerStructName
        _ -> fst <$> structGetToSpir i structVar uniformStructType StorUniform

    tmp <- SpirId <$> nextVar
    sTypeId <- getTypeId (TStruct structName NotUniform)
    output $ OpCompositeConstruct tmp sTypeId ctrVars
    return tmp

-- | Recursively walk through the struct and make it to an uniform version
structToUniformStruct :: String -> SpirCompiler ([FieldDef SpirType], SpirType)
structToUniformStruct structName = do
    fields <- gets $ (Map.! structName) . _structDefs
    let uniformFields     = toUniform <$> fields
        uniformStructName = structName ++ "_Uniform"
        uniformStructType = TStruct uniformStructName Uniform
    modify $ \st -> st { _structDefs = Map.insert uniformStructName uniformFields (_structDefs st) }
    return (uniformFields, uniformStructType)
    where
        toUniform = thirdTriple $ \case
            TStruct innerStructName _ -> TStruct innerStructName Uniform
            t -> t

-- -----------------------------------------------------------------------------
-- * Block code generation
--

-- | Generate code for a labelled block (including phi nodes)
labelledToSpir :: ReturnCallback -> SLabelledBlock -> SpirCompiler ()
labelledToSpir retCallback (SLabelled l phiNodes block) = do
    l' <- getRenamedLabel l
    output $ OpLabel (SpirId l')
    renamedPhiVars <- forM phiNodes $ \(SPhiNode (Var var t) args) -> do
        var'  <- nextVar
        args' <- mapM getRenamed args
        varType <- getTypeId (TPointer StorFunction t)
        output $ OpPhi (SpirId var') varType args'
        modify $ \st -> st { _phiVars = Map.insert var' t (_phiVars st) }
        return (var, var')
    let renameMap = Map.fromList renamedPhiVars
    modify $ \st -> st { _renames = Map.union renameMap (_renames st) }
    blockToSpir retCallback block
    where
        getRenamed (pl, a) = do
            pl' <- getRenamedLabel pl
            a'  <- getRenamedVar a
            return (SpirId a', SpirId pl')

-- | Generate code for one block
blockToSpir :: ReturnCallback -> SBlock -> SpirCompiler ()
blockToSpir retCallback (SBlock stmts) = forM_ stmts (stmtToSpir retCallback True)

-- | Generate code for one statement
stmtToSpir :: ReturnCallback -> Bool -> SStmt -> SpirCompiler ()
stmtToSpir _ newVariable (SAssign (Var var t) expr) = do
    -- var' <- nextVar  CAN BE FORWARD REFERENCED!
    var' <- getRenamedVar var
    varType  <- getTypeId (TPointer StorFunction t)
    (tmp, _) <- exprToSpir expr
    when newVariable (output $ OpVariable (SpirId var') varType StorFunction)
    output $ OpStore (SpirId var') tmp
    insertRenamed var var'
stmtToSpir _ _ (SGoto l) = do
    l' <- getRenamedLabel l
    output $ OpBranch (SpirId l')
stmtToSpir retCallback _ (SReturn expr) = retCallback expr $ do
    (tmp, _) <- exprToSpir expr
    output $ OpReturnValue tmp
stmtToSpir _ _ (SIf sm expr l1 l2) = do
    (tmp, _) <- exprToSpir expr
    l1' <- SpirId <$> getRenamedLabel l1
    l2' <- SpirId <$> getRenamedLabel l2
    output =<< case sm of
        Just (SLoopMerge breakL contL) -> do
            breakL' <- SpirId <$> getRenamedLabel breakL
            contL'  <- SpirId <$> getRenamedLabel contL
            return $ OpLoopMerge breakL' contL' LCNone
        Just (SSelectionMerge l) -> do
            l' <- SpirId <$> getRenamedLabel l
            return $ OpSelectionMerge l' SelCtrNone
        Nothing -> return $ OpSelectionMerge l2' SelCtrNone
    output $ OpBranchConditional tmp l1' l2'

-- | Generate code for one expression
exprToSpir :: SExpr -> SpirCompiler (SpirId, SpirType)
exprToSpir (SVar (Var vName t)) = do
    var' <- SpirId <$> getRenamedVar vName
    tmp  <- SpirId <$> nextVar
    varType <- getTypeId t
    output $ OpLoad tmp varType var'
    return (tmp, t)
exprToSpir (SApp (Var fName fType) args) = do
    let (TFun rt at) = fType
    retType <- getTypeId rt
    argTypes <- mapM getTypeId at
    tmp <- SpirId <$> nextVar
    phiVars <- gets _phiVars
    args' <- mapM getRenamedVar (_varName <$> args)
    args'' <- forM args' $ \arg -> case Map.lookup arg phiVars of
        Just t' -> do
            argTmp <- SpirId <$> nextVar
            argType <- getTypeId t'
            output $ OpLoad argTmp argType (SpirId arg)
            varType <- getTypeId (TPointer StorFunction t')
            varTmp <- SpirId <$> nextVar
            output $ OpVariable varTmp varType StorFunction
            output $ OpStore varTmp argTmp
            return varTmp
        Nothing -> return (SpirId arg)
    case Map.lookup fName spirLibraryList of
        Just fName' -> do
            argTmps <- replicateM (length args'') nextVar
            let argTmps' = SpirId <$> argTmps
            zipWithM_ (\arg (tmp', argType) -> output (OpLoad tmp' argType arg)) 
                args'' (zip argTmps' argTypes)
            extInstId <- getExtInstId "GLSL.std.450"

            output $ if fName == intToFloat 
                then OpConvertSToF tmp retType (head argTmps')
                else if fName == floatToInt 
                    then OpConvertFToS tmp retType (head argTmps')
                    else OpExtInst tmp retType extInstId (fName' rt) argTmps'
        Nothing     ->
            output $ OpFunctionCall tmp retType (SpirId fName) args''
    return (tmp, rt)
exprToSpir (SStructCtr sType vars) = do
    loaded <- forM vars $ \var -> exprToSpir (SVar var)
    tmp <- SpirId <$> nextVar
    sTypeId <- getTypeId sType
    output $ OpCompositeConstruct tmp sTypeId (fst <$> loaded)
    return (tmp, sType)
exprToSpir (SStructGet i (Var struct t)) =
    structGetToSpir i struct t StorFunction
exprToSpir (STupleProj i (Var tuple t)) = do
    vars <- replicateM 2 nextVar
    let [resVar, tmp] = SpirId <$> vars
    tuple' <- SpirId <$> getRenamedVar tuple
    let (TVector t' _) = t
    ptrVarType <- getTypeId (TPointer StorFunction t')
    varType <- getTypeId t'
    (iVar, _) <- getConstId (CUnsignedInt i)

    output $ OpAccessChain resVar ptrVarType tuple' iVar
    output $ OpLoad tmp varType resVar
    return (tmp, t')
exprToSpir (SBinOp op e1 e2) = do
    (t1, et) <- exprToSpir e1
    (t2, _)  <- exprToSpir e2
    v  <- SpirId <$> nextVar
    eType <- getTypeId et
    boolType <- getTypeId $ case et of
        TVector _ i -> TVector TBool i
        _ -> TBool
    let innerExprType = case et of
            TVector iet _ -> iet
            _ -> et
    output $ case (op, innerExprType) of
        (OpAdd, TInt)   -> OpIAdd v eType t1 t2
        (OpAdd, TFloat) -> OpFAdd v eType t1 t2
        (OpMul, TInt)   -> OpIMul v eType t1 t2
        (OpMul, TFloat) -> OpFMul v eType t1 t2
        (OpSub, TInt)   -> OpISub v eType t1 t2
        (OpSub, TFloat) -> OpFSub v eType t1 t2
        (OpDiv, TInt)   -> OpSDiv v eType t1 t2
        (OpDiv, TFloat) -> OpFDiv v eType t1 t2
        (OpMod, TInt)   -> OpSMod v eType t1 t2
        (OpMod, TFloat) -> OpFMod v eType t1 t2
        (OpEq, TBool)   -> OpLogicalEqual v boolType t1 t2
        (OpEq, TInt)    -> OpIEqual v boolType t1 t2
        (OpEq, TFloat)  -> OpFOrdEqual v boolType t1 t2
        (OpNotEq, TBool) -> OpLogicalNotEqual v boolType t1 t2
        (OpNotEq, TInt)    -> OpINotEqual v boolType t1 t2
        (OpNotEq, TFloat)  -> OpFOrdNotEqual v boolType t1 t2
        (OpLT, TInt)    -> OpSLessThan v boolType t1 t2
        (OpLT, TFloat)  -> OpFOrdLessThan v boolType t1 t2
        (OpLTEq, TInt)    -> OpSLessThanEqual v boolType t1 t2
        (OpLTEq, TFloat)  -> OpFOrdLessThanEqual v boolType t1 t2
        (OpGT, TInt)    -> OpSGreaterThan v boolType t1 t2
        (OpGT, TFloat)  -> OpFOrdGreaterThan v boolType t1 t2
        (OpGTEq, TInt)    -> OpSGreaterThanEqual v boolType t1 t2
        (OpGTEq, TFloat)  -> OpFOrdGreaterThanEqual v boolType t1 t2
        (OpAnd, _) -> OpLogicalAnd v boolType t1 t2
        (OpOr, _)  -> OpLogicalOr v boolType t1 t2
        _ -> undefined
    return (v, et)
exprToSpir (SUnOp op e) = do
    (te, t) <- exprToSpir e
    v  <- SpirId <$> nextVar
    numType <- getTypeId t
    boolType  <- getTypeId TBool
    output $ case (op, t) of
        (OpNeg, TFloat) -> OpFNegate v numType te
        (OpNeg, TInt) -> OpSNegate v numType te
        (OpNot, _) -> OpLogicalNot v boolType te
        _ -> undefined
    return (v, t)
exprToSpir (SLitFloat f) = getConstId (CFloat f)
exprToSpir (SLitBool b) = getConstId (CBool b)
exprToSpir (SLitInt i) = getConstId (CSignedInt i)

-- | Helper function generating "getter" for SPIR-V struct (Composite)
structGetToSpir :: Int
                -> String
                -> SpirType
                -> SpirStorageClass
                -> SpirCompiler (SpirId, SpirType)
structGetToSpir i structVar outType ptrStorageClass = do
    vars <- replicateM 2 nextVar
    let [resVar, tmp] = SpirId <$> vars
    tuple' <- SpirId <$> getRenamedVar structVar
    let (TStruct sName _) = outType
    (_, _, fieldTy) <- gets $ (!! i) . (Map.! sName) . _structDefs
    ptrVarType <- getTypeId (TPointer ptrStorageClass fieldTy)
    varType <- getTypeId fieldTy
    (iVar, _) <- getConstId (CSignedInt i)

    output $ OpAccessChain resVar ptrVarType tuple' iVar
    output $ OpLoad tmp varType resVar
    return (tmp, fieldTy)

-- | Converts SSA label to a SPIR-V label variable
getRenamedLabel :: SLabel -> SpirCompiler String
getRenamedLabel (SLabel l) = getRenamedVar (toLabel l)

-- | Adds '@' at the beginning of a label string to differentiate between labels
-- and normal variables
toLabel :: String -> String
toLabel = (ssaToSpirLabelPrefix ++)

-- | Returns `SpirId` for a given extended instruction set
getExtInstId :: String -> SpirCompiler SpirId
getExtInstId extInstSetName = gets _extInstImports >>= \ei -> case Map.lookup extInstSetName ei of
    Just (OpExtInstImport extInstId _) -> return extInstId
    Just _ -> undefined
    Nothing -> do
        extInstId <- SpirId <$> nextVar
        let extInstImport = OpExtInstImport extInstId extInstSetName
            insertFn      = Map.insert extInstSetName extInstImport
        modify $ \st -> st { _extInstImports = insertFn (_extInstImports st) }
        return extInstId

getConstId :: SConst -> SpirCompiler (SpirId, SpirType)
getConstId c@(CBool b) = gets _constants >>= \cs -> case Map.lookup c cs of
    Just (OpConstantTrue constId _, t) -> return (constId, t)
    Just (OpConstantFalse constId _, t) -> return (constId, t)
    Just _ -> undefined
    Nothing -> do
        constId  <- SpirId <$> nextVar
        let boolType = TBool
        boolTypeId <- getTypeId boolType
        let constant = if b 
            then OpConstantTrue constId boolTypeId 
            else OpConstantFalse constId boolTypeId
        modify $ \st -> st 
            { _constants = Map.insert c (constant, boolType) (_constants st) }
        return (constId, boolType)
getConstId c = gets _constants >>= \cs -> case Map.lookup c cs of
    Just (OpConstant constId _ _, t)    -> return (constId, t)
    Just _ -> undefined
    Nothing -> do
        constId <- SpirId <$> nextVar
        ((val, ty), typeId) <- case c of
            CFloat f       -> ((SCFloat f, TFloat), ) <$> getTypeId TFloat
            CSignedInt i   -> ((SCSigned i, TInt), ) <$> getTypeId TInt
            CUnsignedInt i -> ((SCUnsigned i, TUnsignedInt), ) <$> getTypeId TUnsignedInt
            _ -> undefined
        let constant = OpConstant constId typeId val
        modify $ \st -> st 
            { _constants = Map.insert c (constant, ty) (_constants st) }
        return (constId, ty)

-- WALKAROUND: Apparently State, Writer and Data.Map get confused when it comes
-- to sequencing operations.
-- In the State map there was `TVector TFloat _` key but no `TFloat` (yet)
-- and `Map.!` was throwing exceptions inside `typesToOps` function
-- (apparently it started processing (TVector TFloat _) before the end of
-- the State pass). Now "new" getTypeId puts types into the typeMap recursively
-- to ensure that all "composite" types are already in the map
-- | Return SpirId for specific type
getTypeId :: SpirType -> SpirCompiler SpirId
getTypeId t@(TVector inner _) = getTypeId inner >> getTypeId' t
getTypeId t@(TPointer _ inner) = getTypeId inner >> getTypeId' t
getTypeId t@(TFun ret args) =
    getTypeId ret >> mapM_ getTypeId args >> getTypeId' t
getTypeId t@(TStruct sName _isUniform) = do
    fieldDefs <- gets $ (Map.! sName) . _structDefs
    mapM_ getTypeId (trdTriple <$> fieldDefs)
    getTypeId' t
getTypeId t = getTypeId' t

getTypeId' :: SpirType -> SpirCompiler SpirId
getTypeId' t = do
    typeIds <- gets _typeIds
    swap1_3 maybe return (Map.lookup t typeIds) $ do
        tVar <- SpirId <$> nextVar
        let typeIds' = Map.insert t tVar typeIds
        modify $ \st -> st { _typeIds = typeIds' }
        return tVar

-- | Generate code for all types used by the functions
typesToOps :: SpirCompiler [SpirOp]
typesToOps = do
    typeIds    <- gets _typeIds
    structDefs <- gets _structDefs
    types <- forM (Map.toList typeIds) $
        \(t, var) -> case t of
            TBool -> return [OpTypeBool var]
            TInt -> return [OpTypeInt var 32 True]
            TUnsignedInt -> return [OpTypeInt var 32 False]
            TFloat -> return [OpTypeFloat var 32]
            TVector t' size -> return [OpTypeVector var (typeIds Map.! t') size]
            TArray t' size -> do
                iVar     <- SpirId <$> nextVar
                let unsignedNotInMap = isNothing (Map.lookup TUnsignedInt typeIds)
                uintType <- getTypeId TUnsignedInt
                return $ [ OpTypeInt uintType 32 False | unsignedNotInMap ] ++
                         [ OpConstant iVar uintType (SCUnsigned size)
                         , OpTypeArray var (typeIds Map.! t') iVar ]
            TVoid -> return [OpTypeVoid var]
            TPointer storage t' -> return [OpTypePointer var storage (typeIds Map.! t')]
            TFun ret args -> return
                [OpTypeFunction var (typeIds Map.! ret) ((typeIds Map.!) <$> args)]
            TStruct sName _isUniform -> return
                [OpTypeStruct var $ (typeIds Map.!) . trdTriple <$> (structDefs Map.! sName)]
    return (concat types)

-- courtesy of https://stackoverflow.com/a/12131896
-- | Swaps first and third argument of a function
swap1_3 :: (a -> b -> c -> d) -> (b -> c -> a -> d)
swap1_3 = (flip .) . flip
