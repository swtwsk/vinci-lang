{-# LANGUAGE TupleSections #-}
module SPIRV.SSAtoSPIR (ssaToSpir) where

import Control.Monad
import Control.Monad.RWS
import Data.Bifunctor (first, second)
import Data.DList (DList, toList)
import qualified Data.Map as Map

import Core.Ops
import LibraryList (spirLibraryList)
import SSA.AST
import SPIRV.SpirOps
import SPIRV.Types
import Utils.DList (output)
import Utils.VarSupply (fromInfiniteList)

type ReaderEnv = ()

data StateEnv = StateEnv { _typeIds   :: TypeIdsMap
                         , _renames   :: Map.Map String String
                         , _argTypes  :: Map.Map SpirId SpirType
                         , _varSupply :: [String] }

type TypeIdsMap = Map.Map SpirType SpirId
type SpirList = DList SpirOp
type SpirM = RWS ReaderEnv SpirList StateEnv

ssaToSpir :: [SFnDef] -> ([SpirOp], [SpirOp])
ssaToSpir fnDefs = (typeIds ++ consts, fnOps')
    where
        (typeIds, fnOps) = ssaToSpir' fnDefs
        (consts, fnOps') = extractConst fnOps

        extractConst :: [SpirOp] -> ([SpirOp], [SpirOp])
        extractConst (c@OpConstant {}:t) = first (c:) $ extractConst t
        extractConst (h:t) = second (h:) $ extractConst t
        extractConst [] = ([], [])

ssaToSpir' :: [SFnDef] -> ([SpirOp], [SpirOp])
ssaToSpir' fnDefs = (typesToOps $ _typeIds finalSt, toList ops)
    where
        (finalSt, ops) = swap1_3 execRWS readerEnv initialStateEnv $
            mapM_ getTypeId (snd <$> funs) >> mapM_ fnDefToSpir fnDefs

        readerEnv = ()
        initialStateEnv = StateEnv 
            { _typeIds = Map.empty
            , _renames = Map.empty
            , _argTypes = Map.empty
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
    argTypes <- zipWithM processArgs renamedArgs ats
    renamedLabels <- mapM (\(SLabelled (SLabel l) _ _) -> ('@':l, ) <$> nextVar) labelled
    let renameMap = Map.fromList (renamedArgs ++ renamedLabels)

    label <- nextVar
    output $ OpLabel (SpirId label)
    let renameMap' = Map.insert ('@':fName ++ "_init") label renameMap
    modify $ \st -> st { _renames  = renameMap'
                       , _argTypes = Map.fromList argTypes }

    blockToSpir block
    forM_ labelled labelledToSpir

    output OpFunctionEnd
    where
        processArgs :: (VarName, String) -> SpirType -> SpirM (SpirId, SpirType)
        processArgs (_, rArg) t = do
            typeId <- getTypeId t
            let argId = SpirId rArg
            output $ OpFunctionParameter argId typeId
            return (argId, t)

labelledToSpir :: SLabelledBlock -> SpirM ()
labelledToSpir (SLabelled l phiNodes block) = do
    l' <- getRenamedLabel l
    output $ OpLabel (SpirId l')
    renamedPhiVars <- forM phiNodes $ \(SPhiNode (Var var t) args) -> do
        var'  <- nextVar
        args' <- mapM getRenamed args
        varType <- getTypeId (TPointer StorFunction t)
        output $ OpPhi (SpirId var') varType args'
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
    varType <- getTypeId (TPointer StorFunction t)
    tmp  <- exprToSpir expr
    output $ OpVariable (SpirId var') varType StorFunction
    output $ OpStore (SpirId var') tmp
    insertRenamed var var'
stmtToSpir (SGoto l) = do
    l' <- getRenamedLabel l
    output $ OpBranch (SpirId l')
stmtToSpir (SReturn expr) = do
    tmp <- exprToSpir expr
    output $ OpReturnValue tmp
stmtToSpir (SIf expr l1 l2) = do
    tmp <- exprToSpir expr
    l1' <- getRenamedLabel l1
    l2' <- getRenamedLabel l2
    output $ OpBranchConditional tmp (SpirId l1') (SpirId l2')

exprToSpir :: SExpr -> SpirM SpirId
exprToSpir (SVar (Var vName t)) = do
    var' <- SpirId <$> getRenamedVar vName
    tmp  <- SpirId <$> nextVar
    varType <- getTypeId t
    output $ OpLoad tmp varType var'
    return tmp
exprToSpir (SApp (Var fName fType) args) = do
    let (TFun rt _) = fType
    retType <- getTypeId rt
    tmp <- SpirId <$> nextVar
    args' <- mapM getRenamedVar (_varName <$> args)
    -- TODO: OpExtInst is looking up for "extension", should not be hardcoded
    output $ case Map.lookup fName spirLibraryList of
        Just fName' -> OpExtInst tmp retType (SpirId "1") fName' (SpirId <$> args')
        Nothing     -> OpFunctionCall tmp retType (SpirId fName) (SpirId <$> args')
    return tmp
exprToSpir (STupleCtr vars) = do
    loaded <- forM vars $ \var -> exprToSpir (SVar var)
    tmp <- SpirId <$> nextVar
    retType <- getTypeId (TVector TFloat $ length vars)
    output $ OpCompositeConstruct tmp retType loaded
    return tmp
exprToSpir (STupleProj i (Var tuple t)) = do
    vars <- replicateM 3 nextVar
    let [iVar, resVar, tmp] = SpirId <$> vars
    uintType <- getTypeId TUnsignedInt
    tuple' <- SpirId <$> getRenamedVar tuple
    let (TVector t' _) = t
    varType <- getTypeId t'

    output $ OpConstant iVar uintType (SCUnsigned i)
    output $ OpAccessChain resVar varType tuple' iVar
    output $ OpLoad tmp varType resVar
    return resVar
exprToSpir (SBinOp op e1 e2) = do
    t1 <- exprToSpir e1
    t2 <- exprToSpir e2
    v  <- SpirId <$> nextVar
    floatType <- getTypeId TFloat
    boolType  <- getTypeId TBool
    output $ case op of
        OpAdd -> OpFAdd v floatType t1 t2
        OpMul -> OpFMul v floatType t1 t2
        OpSub -> OpFSub v floatType t1 t2
        OpDiv -> OpFDiv v floatType t1 t2
        OpMod -> OpFMod v floatType t1 t2
        OpAnd -> OpLogicalAnd v boolType t1 t2
        OpOr -> OpLogicalOr v boolType t1 t2
        OpEq -> OpFOrdEqual v floatType t1 t2
        OpLT -> OpFOrdLessThan v floatType t1 t2
    return v
exprToSpir (SUnOp op e) = do
    te <- exprToSpir e
    v  <- SpirId <$> nextVar
    floatType <- getTypeId TFloat
    boolType  <- getTypeId TBool
    output $ case op of
        OpNeg -> OpFNegate v floatType te
        OpNot -> OpLogicalNot v boolType te
    return v
exprToSpir (SLitFloat f) = do
    v <- SpirId <$> nextVar
    floatType <- getTypeId TFloat
    output $ OpConstant v floatType (SCFloat f)
    return v
exprToSpir (SLitBool b) = do
    v <- SpirId <$> nextVar
    floatType <- getTypeId TBool
    output $ OpConstant v floatType (SCBool b)
    return v

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
getRenamedLabel (SLabel l) = getRenamedVar ('@':l)

-- WALKAROUND: Apparently State, Writer and Data.Map get confused when it comes
-- to sequencing operations.
-- In the State map there was `TVector TFloat _` key but no `TFloat` (yet)
-- and `Map.!` was throwing exceptions inside `typesToOps` function
-- (apparently it started processing (TVector TFloat _) before the end of
-- the State pass). Now "new" getTypeId puts types into the typeMap recursively
-- to ensure that all "composite" types are already in the map
getTypeId :: SpirType -> SpirM SpirId
getTypeId t@(TVector inner _) = getTypeId' inner >> getTypeId' t
getTypeId t@(TPointer _ inner) = getTypeId' inner >> getTypeId' t
getTypeId t@(TFun ret args) = 
    getTypeId' ret >> mapM_ getTypeId' args >> getTypeId' t
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
typesToOps :: TypeIdsMap -> [SpirOp]
typesToOps typeIds = flip fmap (Map.toList typeIds) $ \(t, var) -> case t of
    TBool -> OpTypeBool var
    TInt -> OpTypeInt var 32 True
    TUnsignedInt -> OpTypeInt var 32 False
    TFloat -> OpTypeFloat var 32
    TVector t' size -> OpTypeVector var (typeIds Map.! t') size
    TVoid -> OpTypeVoid var
    TPointer storage t' -> OpTypePointer var storage (typeIds Map.! t')
    TFun ret args -> 
        OpTypeFunction var (typeIds Map.! ret) ((typeIds Map.!) <$> args)

-- courtesy of https://stackoverflow.com/a/12131896
swap1_3 :: (a -> b -> c -> d) -> (b -> c -> a -> d)
swap1_3 = (flip .) . flip
