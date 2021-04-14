{-# LANGUAGE TupleSections #-}
module SPIRV.SSAtoSPIR (ssaToSpir) where

import Control.Monad
import Control.Monad.RWS
import Data.Bifunctor (first, second)
import Data.DList (DList, toList)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Core.Ops
import SSA.AST
import SPIRV.LibraryList (libraryList)
import SPIRV.SpirOps
import SPIRV.Types
import Utils.DList (output)
import Utils.VarSupply (fromInfiniteList)

data ReaderEnv = ReaderEnv { _funs :: Map.Map String SpirType }

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
        (finalSt, ops) = execRWS (mapM_ fnDefToSpir fnDefs) readerEnv stWithTypes

        readerEnv = ReaderEnv { _funs = libraryList `Map.union` Map.fromList funs }
        (stWithTypes, _) = swap1_3 execRWS readerEnv initialStateEnv $
            mapM_ getTypeId (snd <$> funs)
        
        initialStateEnv = StateEnv 
            { _typeIds = Map.empty
            , _renames = Map.empty
            , _argTypes = Map.empty
            , _varSupply = [show i | i <- [(61 :: Int)..]] }
        funs = flip fmap fnDefs $ \(SFnDef fName args _ _) -> 
                (fName, TFun TFloat (TPointer StorFunction TFloat <$ args))

fnDefToSpir :: SFnDef -> SpirM ()
fnDefToSpir (SFnDef fName fArgs block labelled) = do
    ft <- asks ((Map.! fName) . _funs)
    let (TFun rt ats) = ft
    retType  <- getTypeId rt
    funType  <- getTypeId ft
    output $ OpFunction (SpirId fName) retType FCNone funType

    renamedArgs <- mapM (\(SArg arg) -> (arg, ) <$> buildVar ((arg ++ "_")++)) fArgs
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
    renamedPhiVars <- forM phiNodes $ \(SPhiNode var args) -> do
        var'  <- nextVar
        args' <- mapM getRenamed args
        varType <- getVarTypeId (SpirId var')
        output $ OpPhi (SpirId var') varType args'
        return (var, var')
    let renameMap = Map.fromList renamedPhiVars
    modify $ \st -> st { _renames = Map.union renameMap (_renames st) }
    blockToSpir block
    where
        getRenamed (pl, SArg a) = do
            pl' <- getRenamedLabel pl
            a'  <- getRenamedVar a
            return (SpirId a', SpirId pl')

blockToSpir :: SBlock -> SpirM ()
blockToSpir (SBlock stmts) = forM_ stmts stmtToSpir

stmtToSpir :: SStmt -> SpirM ()
stmtToSpir (SAssign var expr) = do
    -- var' <- nextVar  CAN BE FORWARD REFERENCED!
    var' <- getRenamedVar var
    varType <- getVarTypeId (SpirId var')
    tmp  <- exprToSpir expr Nothing
    output $ OpVariable (SpirId var') varType StorFunction
    output $ OpStore (SpirId var') tmp
    insertRenamed var var'
stmtToSpir (SGoto l) = do
    l' <- getRenamedLabel l
    output $ OpBranch (SpirId l')
stmtToSpir (SReturn expr) = do
    tmp <- exprToSpir expr Nothing
    output $ OpReturnValue tmp
stmtToSpir (SIf expr l1 l2) = do
    tmp <- exprToSpir expr (Just TBool)
    l1' <- getRenamedLabel l1
    l2' <- getRenamedLabel l2
    output $ OpBranchConditional tmp (SpirId l1') (SpirId l2')

exprToSpir :: SExpr -> Maybe SpirType -> SpirM SpirId
exprToSpir (SVar var) t = do
    var' <- SpirId <$> getRenamedVar var
    tmp  <- SpirId <$> nextVar
    varType <- getVarTypeIdWithType var' (fromMaybe TFloat t)
    output $ OpLoad tmp varType var'
    return tmp
exprToSpir (SApp fName args) _ = do
    funType <- asks ((Map.! fName) . _funs)
    let (TFun rt _) = funType
    retType <- getTypeId rt
    tmp <- SpirId <$> nextVar
    args' <- mapM getRenamedVar args
    output $ OpFunctionCall tmp retType (SpirId fName) (SpirId <$> args')
    return tmp
exprToSpir (SBinOp op e1 e2) _ = do
    let varType = case op of
            OpAnd -> TBool
            OpOr  -> TBool
            _     -> TFloat
    t1 <- exprToSpir e1 (Just varType)
    t2 <- exprToSpir e2 (Just varType)
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
exprToSpir (SUnOp op e) _ = do
    te <- exprToSpir e (Just $ if op == OpNeg then TFloat else TBool)
    v  <- SpirId <$> nextVar
    floatType <- getTypeId TFloat
    boolType  <- getTypeId TBool
    output $ case op of
        OpNeg -> OpFNegate v floatType te
        OpNot -> OpLogicalNot v boolType te
    return v
exprToSpir (SLitFloat f) _ = do
    v <- SpirId <$> nextVar
    floatType <- getTypeId TFloat
    output $ OpConstant v floatType (SCFloat f)
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

getVarTypeId :: SpirId -> SpirM SpirId
getVarTypeId var = getVarTypeIdWithType var TFloat

getVarTypeIdWithType :: SpirId -> SpirType -> SpirM SpirId
getVarTypeIdWithType var t = do
    argTypes <- gets _argTypes
    getTypeId $ 
        fromMaybe (TPointer StorFunction t) (Map.lookup var argTypes)

getTypeId :: SpirType -> SpirM SpirId
getTypeId t = do
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
