{-# LANGUAGE TupleSections #-}
module SPIRV.SSAtoSPIR (ssaToSpir) where

import Control.Monad
import Control.Monad.RWS
import Data.Bifunctor (first, second)
import Data.DList (DList, toList)
import qualified Data.Map as Map

import Core.Ops
import LibraryList (spirLibraryList)
import ManglingPrefixes (ssaToSpirLabelPrefix)
import StructDefMap (StructDefMap)
import SSA.AST
import SPIRV.SpirOps
import SPIRV.Types
import Utils.DList (output)
import Utils.VarSupply (fromInfiniteList)

type ReaderEnv = StructDefMap SpirType

data StateEnv = StateEnv { _typeIds   :: TypeIdsMap
                         , _renames   :: Map.Map String String
                         , _phiVars   :: Map.Map String SpirType
                         , _varSupply :: [String] }

type TypeIdsMap = Map.Map SpirType SpirId
type SpirList = DList SpirOp
type SpirM = RWS ReaderEnv SpirList StateEnv

ssaToSpir :: ([SFnDef], StructDefMap SpirType) -> ([SpirOp], [SpirOp])
ssaToSpir (fnDefs, structDefs) = (typeIds ++ consts, fnOps'')
    where
        (typeIds, fnOps) = ssaToSpir' fnDefs structDefs
        (consts, fnOps') = extractConst fnOps
        fnOps''          = hoistVariables fnOps'

        extractConst :: [SpirOp] -> ([SpirOp], [SpirOp])
        extractConst (c@OpConstant {}:t) = first (c:) $ extractConst t
        extractConst (c@OpConstantTrue {}:t) = first (c:) $ extractConst t
        extractConst (c@OpConstantFalse {}:t) = first (c:) $ extractConst t
        extractConst (h:t) = second (h:) $ extractConst t
        extractConst [] = ([], [])

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

ssaToSpir' :: [SFnDef] -> StructDefMap SpirType -> ([SpirOp], [SpirOp])
ssaToSpir' fnDefs structDefs =
    (typesToOps structDefs $ _typeIds finalSt, toList ops)
    where
        (finalSt, ops) = swap1_3 execRWS readerEnv initialStateEnv $
            mapM_ getTypeId (snd <$> funs) >> mapM_ fnDefToSpir fnDefs

        readerEnv = structDefs
        initialStateEnv = StateEnv
            { _typeIds = Map.empty
            , _renames = Map.empty
            , _phiVars = Map.empty
            , _varSupply = [show i | i <- [(61 :: Int)..]] }
        funs = flip fmap fnDefs $ \(SFnDef fName rt args _ _) ->
                (fName, TFun rt ((\(SArg (Var _ t)) -> TPointer StorFunction t) <$> args))

fnDefToSpir :: SFnDef -> SpirM ()
fnDefToSpir (SFnDef fName rt fArgs block labelled) = do
    retType  <- getTypeId rt
    let ats  = (\(SArg a) -> TPointer StorFunction $ _varType a) <$> fArgs
    funType  <- getTypeId $ TFun rt ats
    output $ OpFunction (SpirId fName) retType FCNone funType

    renamedArgs <- forM fArgs $ \(SArg (Var arg _)) ->
                        (arg, ) <$> buildVar ((arg ++ "_")++)
    zipWithM_ processArgs renamedArgs ats
    renamedLabels <- mapM (\(SLabelled (SLabel l) _ _) -> (toLabel l, ) <$> nextVar) labelled
    let renameMap = Map.fromList (renamedArgs ++ renamedLabels)

    label <- nextVar
    output $ OpLabel (SpirId label)
    let renameMap' = Map.insert (toLabel $ fName ++ "_init") label renameMap
    modify $ \st -> st { _renames  = renameMap' }

    blockToSpir block
    forM_ labelled labelledToSpir

    output OpFunctionEnd
    where
        processArgs :: (VarName, String) -> SpirType -> SpirM ()
        processArgs (_, rArg) t = do
            typeId <- getTypeId t
            let argId = SpirId rArg
            output $ OpFunctionParameter argId typeId

labelledToSpir :: SLabelledBlock -> SpirM ()
labelledToSpir (SLabelled l phiNodes block) = do
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
    blockToSpir block
    where
        getRenamed (pl, a) = do
            pl' <- getRenamedLabel pl
            a'  <- getRenamedVar a
            return (SpirId a', SpirId pl')

blockToSpir :: SBlock -> SpirM ()
blockToSpir (SBlock stmts) = forM_ stmts stmtToSpir

stmtToSpir :: SStmt -> SpirM ()
stmtToSpir (SAssign (Var var t) expr) = do
    -- var' <- nextVar  CAN BE FORWARD REFERENCED!
    var' <- getRenamedVar var
    varType  <- getTypeId (TPointer StorFunction t)
    (tmp, _) <- exprToSpir expr
    output $ OpVariable (SpirId var') varType StorFunction
    output $ OpStore (SpirId var') tmp
    insertRenamed var var'
stmtToSpir (SGoto l) = do
    l' <- getRenamedLabel l
    output $ OpBranch (SpirId l')
stmtToSpir (SReturn expr) = do
    (tmp, _) <- exprToSpir expr
    output $ OpReturnValue tmp
stmtToSpir (SIf sm expr l1 l2) = do
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

exprToSpir :: SExpr -> SpirM (SpirId, SpirType)
exprToSpir (SVar (Var vName t)) = do
    var' <- SpirId <$> getRenamedVar vName
    tmp  <- SpirId <$> nextVar
    varType <- getTypeId t
    output $ OpLoad tmp varType var'
    return (tmp, t)
exprToSpir (SApp (Var fName fType) args) = do
    let (TFun rt _) = fType
    retType <- getTypeId rt
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
            zipWithM_ (\arg tmp' -> output (OpLoad tmp' retType arg))
                args'' (SpirId <$> argTmps)
            -- TODO: OpExtInst is looking up for "extension", should not be hardcoded
            output $ OpExtInst tmp retType (SpirId "1") fName' (SpirId <$> argTmps)
        Nothing     ->
            output $ OpFunctionCall tmp retType (SpirId fName) args''
    return (tmp, rt)
exprToSpir (SStructCtr sType vars) = do
    loaded <- forM vars $ \var -> exprToSpir (SVar var)
    tmp <- SpirId <$> nextVar
    sTypeId <- getTypeId sType
    output $ OpCompositeConstruct tmp sTypeId (fst <$> loaded)
    return (tmp, sType)
exprToSpir (SStructGet i (Var struct t)) = do
    vars <- replicateM 3 nextVar
    let [iVar, resVar, tmp] = SpirId <$> vars
    intType <- getTypeId TInt
    tuple' <- SpirId <$> getRenamedVar struct
    let (TStruct sName) = t
    (_, fieldTy) <- asks ((!! i) . (Map.! sName))
    ptrVarType <- getTypeId (TPointer StorFunction fieldTy)
    varType <- getTypeId fieldTy

    output $ OpConstant iVar intType (SCSigned i)
    output $ OpAccessChain resVar ptrVarType tuple' iVar
    output $ OpLoad tmp varType resVar
    return (tmp, fieldTy)
exprToSpir (STupleProj i (Var tuple t)) = do
    vars <- replicateM 3 nextVar
    let [iVar, resVar, tmp] = SpirId <$> vars
    uintType <- getTypeId TUnsignedInt
    tuple' <- SpirId <$> getRenamedVar tuple
    let (TVector t' _) = t
    ptrVarType <- getTypeId (TPointer StorFunction t')
    varType <- getTypeId t'

    output $ OpConstant iVar uintType (SCUnsigned i)
    output $ OpAccessChain resVar ptrVarType tuple' iVar
    output $ OpLoad tmp varType resVar
    return (tmp, t')
exprToSpir (SBinOp op e1 e2) = do
    (t1, et) <- exprToSpir e1
    (t2, _) <- exprToSpir e2
    v  <- SpirId <$> nextVar
    eType <- getTypeId et
    boolType <- getTypeId TBool
    output $ case (op, et) of
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
        (OpLT, TInt)    -> OpSLessThan v boolType t1 t2
        (OpLT, TFloat)  -> OpFOrdLessThan v boolType t1 t2
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
exprToSpir (SLitFloat f) = do
    v <- SpirId <$> nextVar
    floatType <- getTypeId TFloat
    output $ OpConstant v floatType (SCFloat f)
    return (v, TFloat)
exprToSpir (SLitBool b) = do
    v <- SpirId <$> nextVar
    boolType <- getTypeId TBool
    output $ if b then OpConstantTrue v boolType else OpConstantFalse v boolType
    return (v, TBool)
exprToSpir (SLitInt i) = do
    v <- SpirId <$> nextVar
    intType <- getTypeId TInt
    output $ OpConstant v intType (SCSigned i)
    return (v, TInt)

insertRenamed :: String -> String -> SpirM ()
insertRenamed var renamedVar = do
    renames <- gets _renames
    let renames' = Map.insert var renamedVar renames
    modify $ \st -> st { _renames = renames' }

getRenamedVar :: String -> SpirM String
getRenamedVar var = do
    renames <- gets _renames
    swap1_3 maybe return (Map.lookup var renames) $ do
        var' <- nextVar
        insertRenamed var var'
        return var'

getRenamedLabel :: SLabel -> SpirM String
getRenamedLabel (SLabel l) = getRenamedVar (toLabel l)

toLabel :: String -> String
toLabel = (ssaToSpirLabelPrefix ++)

-- WALKAROUND: Apparently State, Writer and Data.Map get confused when it comes
-- to sequencing operations.
-- In the State map there was `TVector TFloat _` key but no `TFloat` (yet)
-- and `Map.!` was throwing exceptions inside `typesToOps` function
-- (apparently it started processing (TVector TFloat _) before the end of
-- the State pass). Now "new" getTypeId puts types into the typeMap recursively
-- to ensure that all "composite" types are already in the map
getTypeId :: SpirType -> SpirM SpirId
getTypeId t@(TVector inner _) = getTypeId inner >> getTypeId' t
getTypeId t@(TPointer _ inner) = getTypeId inner >> getTypeId' t
getTypeId t@(TFun ret args) =
    getTypeId ret >> mapM_ getTypeId args >> getTypeId' t
getTypeId t@(TStruct sName) = do
    fieldDefs <- asks (Map.! sName)
    mapM_ getTypeId (snd <$> fieldDefs)
    getTypeId' t
getTypeId t = getTypeId' t

getTypeId' :: SpirType -> SpirM SpirId
getTypeId' t = do
    typeIds <- gets _typeIds
    swap1_3 maybe return (Map.lookup t typeIds) $ do
        tVar <- SpirId <$> nextVar
        let typeIds' = Map.insert t tVar typeIds
        modify $ \st -> st { _typeIds = typeIds' }
        return tVar

nextVar :: SpirM String
nextVar = do
    (x, xs) <- gets (fromInfiniteList . _varSupply)
    modify $ \s -> s { _varSupply = xs }
    return x

buildVar :: (String -> String) -> SpirM String
buildVar f = fmap f nextVar

-- walk types
typesToOps :: StructDefMap SpirType -> TypeIdsMap -> [SpirOp]
typesToOps structDefs typeIds = flip fmap (Map.toList typeIds) $
    \(t, var) -> case t of
        TBool -> OpTypeBool var
        TInt -> OpTypeInt var 32 True
        TUnsignedInt -> OpTypeInt var 32 False
        TFloat -> OpTypeFloat var 32
        TVector t' size -> OpTypeVector var (typeIds Map.! t') size
        TVoid -> OpTypeVoid var
        TPointer storage t' -> OpTypePointer var storage (typeIds Map.! t')
        TFun ret args ->
            OpTypeFunction var (typeIds Map.! ret) ((typeIds Map.!) <$> args)
        TStruct sName -> OpTypeStruct var $
            (typeIds Map.!) . snd <$> (structDefs Map.! sName)

-- courtesy of https://stackoverflow.com/a/12131896
swap1_3 :: (a -> b -> c -> d) -> (b -> c -> a -> d)
swap1_3 = (flip .) . flip
