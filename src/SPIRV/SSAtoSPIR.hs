{-# LANGUAGE TupleSections #-}
module SPIRV.SSAtoSPIR (ssaToSpir) where

import Control.Monad
import Control.Monad.RWS
import Data.DList (DList, toList)
import qualified Data.Map as Map

import Core.Ops
import SSA.AST
import SPIRV.SpirOps
import Utils.DList (output)
import Utils.VarSupply (fromInfiniteList)

data SpirType = SpirFloat 
              | SpirFloatParameter
              | SpirFn SpirType [SpirType]
              deriving (Eq, Ord)

type RenameMap = () --Map.Map String String

data StateEnv = StateEnv { _typeIds :: Map.Map SpirType SpirId
                         , _renames :: Map.Map String String
                         , _varSupply :: [String] }

type SpirList = DList SpirOp
type SpirM = RWS RenameMap SpirList StateEnv

ssaToSpir :: SFnDef -> [SpirOp]
ssaToSpir fnDef = toList ops
    where
        (_st, ops) = execRWS (fnDefToSpir fnDef) () initialStateEnv

fnDefToSpir :: SFnDef -> SpirM ()
fnDefToSpir (SFnDef fName fArgs block labelled) = do
    typeIds <- gets _typeIds
    let floatTypeId          = typeIds Map.! SpirFloat
        floatParameterTypeId = typeIds Map.! SpirFloatParameter
        floatToFloatTypeId   = typeIds Map.! floatToFloatFunType
    output $ OpFunction (SpirId $ fName ++ "_f1_") floatTypeId FCNone floatToFloatTypeId

    renamedArgs <- mapM (\(SArg arg) -> (arg, ) <$> buildVar ((arg ++ "_")++)) fArgs
    forM_ renamedArgs (\(_, rArg) -> 
        output $ OpFunctionParameter (SpirId rArg) floatParameterTypeId)
    renamedLabels <- mapM (\(SLabelled (SLabel l) _ _) -> ('@':l, ) <$> nextVar) labelled
    let renameMap = Map.fromList (renamedArgs ++ renamedLabels)

    label <- nextVar
    output $ OpLabel (SpirId label)
    let renameMap' = Map.insert ('@':fName ++ "_init") label renameMap
    modify $ \st -> st { _renames = Map.union renameMap' (_renames st) }

    blockToSpir block
    forM_ labelled labelledToSpir

    output OpFunctionEnd

labelledToSpir :: SLabelledBlock -> SpirM ()
labelledToSpir (SLabelled l phiNodes block) = do
    l' <- getRenamedLabel l
    varType <- gets $ flip (Map.!) SpirFloat . _typeIds
    output $ OpLabel (SpirId l')
    renamedPhiVars <- forM phiNodes $ \(SPhiNode var args) -> do
        var'  <- nextVar
        args' <- mapM getRenamed args
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
    var' <- nextVar
    floatParamType <- gets $ flip (Map.!) SpirFloatParameter . _typeIds
    tmp  <- exprToSpir expr
    output $ OpVariable (SpirId var') floatParamType StorFunction
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
exprToSpir (SVar var) = do
    var' <- SpirId <$> getRenamedVar var
    tmp  <- SpirId <$> nextVar
    floatType <- gets $ flip (Map.!) SpirFloat . _typeIds
    output $ OpLoad tmp floatType var'
    return tmp
exprToSpir (SApp _fName _args) = undefined
exprToSpir (SBinOp op e1 e2) = do
    t1 <- exprToSpir e1
    t2 <- exprToSpir e2
    v  <- SpirId <$> nextVar
    floatType <- gets $ flip (Map.!) SpirFloat . _typeIds
    output $ case op of
        OpAdd -> OpFAdd v floatType t1 t2
        OpMul -> OpFMul v floatType t1 t2
        OpSub -> OpFSub v floatType t1 t2
        OpDiv -> OpFDiv v floatType t1 t2
        -- OpAnd -> OpLogicalAnd v boolType t1 t2
        -- OpOr -> OpLogicalOr v boolType t1 t2
        OpEq -> OpFOrdEqual v floatType t1 t2
        OpLT -> OpFOrdLessThan v floatType t1 t2
        _ -> undefined
    return v
exprToSpir (SUnOp _op _e) = undefined
exprToSpir (SLitFloat f) = do
    v <- SpirId <$> nextVar
    floatType <- gets $ flip (Map.!) SpirFloat . _typeIds
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

nextVar :: SpirM String
nextVar = do
    (x, xs) <- gets (fromInfiniteList . _varSupply)
    modify $ \s -> s { _varSupply = xs }
    return x

buildVar :: (String -> String) -> SpirM String
buildVar f = fmap f nextVar

initialStateEnv :: StateEnv
initialStateEnv = StateEnv 
    { _typeIds = Map.fromList 
        [ (SpirFloat, SpirId "float")
        , (SpirFloatParameter, SpirId "_ptr_Function_float")
        , (floatToFloatFunType, SpirId "8")]
    , _renames = Map.empty
    , _varSupply = [show i | i <- [(53 :: Int)..]] } --[(11 :: Int)..]] }

floatToFloatFunType :: SpirType
floatToFloatFunType = SpirFn SpirFloat [SpirFloatParameter]

-- courtesy of https://stackoverflow.com/a/12131896
swap1_3 :: (a -> b -> c -> d) -> (b -> c -> a -> d)
swap1_3 = (flip .) . flip
