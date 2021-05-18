{-# LANGUAGE TupleSections #-}
module SSA.CPStoSSA (cpsToSSA, cpsFunToSSA) where

import Control.Monad.RWS
import Data.Bifunctor
import Data.DList (DList, toList)
import Data.List (findIndex)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as Map

import qualified CPS.AST as CPS
import ManglingPrefixes (cpsToSsaVarPrefix)
import SSA.AST
import StructDefMap (StructDefMap)
import SPIRV.Types
import Utils.DList (output)
import Utils.VarSupply (fromInfiniteList)

type Label = String
type StmtList = DList SStmt

type PhiMap = Map.Map Label [[(Label, String)]]  -- label: [_1 <- [(l, s)], ...]
data JumpCont = LocalCont [Var] CPS.CExpr
              | ReturnJumpCont [Var] CPS.CExpr

data ReaderEnv = ReaderEnv { _funCont      :: (String, String)
                           , _currentLabel :: Label
                           , _structDefs   :: StructDefMap CPS.CType }

data StateEnv = StateEnv { _untranspiledJumps :: [(Label, JumpCont, ReaderEnv)]
                         , _phiValues :: PhiMap
                         , _varSupply :: [String] }

type TranspileT = RWS ReaderEnv StmtList StateEnv

cpsToSSA :: ([CPS.CFunDef], StructDefMap CPS.CType) 
         -> ([SFnDef], StructDefMap SpirType)
cpsToSSA (funDefs, structDefs) = (fnDefs, spirStructDefs)
    where
        fnDefs = cFunToSSA structDefs <$> funDefs
        spirStructDefs = Map.map (second cTypeToSSA <$>) structDefs

cpsFunToSSA :: CPS.CFunDef -> SFnDef
cpsFunToSSA = cFunToSSA Map.empty

cFunToSSA :: StructDefMap CPS.CType -> CPS.CFunDef -> SFnDef
cFunToSSA structDefs (CPS.CFunDef (CPS.Var fName t) k args expr) =
    SFnDef fName rt (SArg <$> args') (SBlock [SGoto $ SLabel initLabel2]) labelled'
    where
        (TFun rt _) = cTypeToSSA t
        args' = varToSSA <$> args
        initLabel = fName ++ "_init"
        initLabel2 = initLabel ++ "2"
        initPhis = (\arg -> [(initLabel, CPS._varName arg)]) <$> args
        r = ReaderEnv { _funCont = (k, fName)
                      , _currentLabel = initLabel
                      , _structDefs = structDefs }
        st = emptyState { _untranspiledJumps = [(initLabel2, ReturnJumpCont args' expr, r)]
                        , _phiValues = Map.singleton initLabel2 initPhis }
        (endState, labelled) = evalJumpsToSSA st
        labelled' = updatePhiNodes labelled endState

cExprToSSA :: CPS.CExpr -> TranspileT ()
cExprToSSA (CPS.CLetVal x val cexpr) = do
    output . SAssign (varToSSA x) $ case val of
        CPS.CLitFloat f        -> SLitFloat f
        CPS.CLitBool b         -> SLitBool b
        CPS.CLitInt i          -> SLitInt i
        CPS.CStruct sName vars -> 
            SStructCtr (TStruct sName NotUniform) $ varToSSA <$> vars
        CPS.CTuple vars        -> 
            SStructCtr (TVector TFloat $ length vars) $ varToSSA <$> vars
    cExprToSSA cexpr
cExprToSSA (CPS.CLetProj x i tuple cexpr) = do
    output $ SAssign (varToSSA x) (STupleProj i $ varToSSA tuple)
    cExprToSSA cexpr
cExprToSSA (CPS.CLetFieldGet x field struct@(CPS.Var _ t) cexpr) = do
    let (CPS.CTStruct sName) = t
    fieldDefs <- asks $ (Map.! sName) . _structDefs
    let i = fromJust (findIndex ((== field) . fst) fieldDefs)
    output $ SAssign (varToSSA x) (SStructGet i $ varToSSA struct)
    cExprToSSA cexpr
cExprToSSA (CPS.CLetCont k x c1 c2) = do
    jumps <- gets _untranspiledJumps
    closure <- ask
    let jumps' = (k, LocalCont [varToSSA x] c1, closure):jumps
    modify (\ts -> ts { _untranspiledJumps = jumps' })
    cExprToSSA c2
cExprToSSA (CPS.CAppCont k x) = do
    isReturnBounded <- asks ((k ==) . fst . _funCont)
    if isReturnBounded 
        then output $ SReturn (SVar $ varToSSA x) 
        else updatePhiAndGoto k [CPS._varName x]
cExprToSSA (CPS.CAppFun (CPS.Var f t) k args) = do
    (fnK, fnName) <- asks _funCont
    if fnK == k
        then if f == fnName
            then updatePhiAndGoto (f ++ "_init2") (CPS._varName <$> args) -- + phiNode f_arg1 -> x
            else callFnAndReturn
        else callFnAndJump
    where
        callFn = do
            let ft@(TFun resT _) = cTypeToSSA t
            v <- (`Var` resT) <$> nextVar
            output $ SAssign v (SApp (Var f ft) (varToSSA <$> args))
            return v
        callFnAndReturn = callFn >>= output .SReturn . SVar
        callFnAndJump = do
            v <- callFn
            updatePhiAndGoto k [_varName v]
cExprToSSA (CPS.CLetPrim x (CPS.CBinOp op) [a, b] cexpr) = do
    let (a', b') = (varToSSA a, varToSSA b)
    output $ SAssign (varToSSA x) (SBinOp op (SVar a') (SVar b'))
    cExprToSSA cexpr
cExprToSSA (CPS.CLetPrim x (CPS.CUnOp op) [a] cexpr) = do
    let a' = varToSSA a
    output $ SAssign (varToSSA x) (SUnOp op (SVar a'))
    cExprToSSA cexpr
cExprToSSA CPS.CLetPrim {} = undefined
cExprToSSA (CPS.CIf x k1 k2) = output $
    SIf Nothing (SVar $ varToSSA x) (SLabel k1) (SLabel k2)
cExprToSSA (CPS.CLetFun _fdef _cexpr) = undefined

evalJumpsToSSA :: StateEnv -> (StateEnv, [SLabelledBlock])
evalJumpsToSSA ts = case _untranspiledJumps ts of
    (l, cont, r):t ->
        let (expr', args') = case cont of
                LocalCont args expr -> (expr, args)
                ReturnJumpCont args expr -> (expr, args)
            r'  = r { _currentLabel = l }
            ts' = ts { _untranspiledJumps = t }
            (newTs, blockExprs) = execRWS (cExprToSSA expr') r' ts'
            emptyPhiNodes = (`SPhiNode` []) <$> args'
            (tailState, tailBlocks) = evalJumpsToSSA newTs in
        (tailState, SLabelled (SLabel l) emptyPhiNodes (SBlock $ toList blockExprs) : tailBlocks)
    [] -> (ts, [])

updatePhiNodes :: [SLabelledBlock] -> StateEnv -> [SLabelledBlock]
updatePhiNodes ((SLabelled l@(SLabel l') phiNodes b):t) st = labelled' : updatePhiNodes t st
    where
        labelled' = SLabelled l (zipAppend phiNodes phiVars) b
        phiVars = fromMaybe [] . Map.lookup l' . _phiValues $ st
        zipAppend :: [SPhiNode] -> [[(Label, String)]] -> [SPhiNode]
        zipAppend (SPhiNode var _:t1) (h2:t2) =
            SPhiNode var (first SLabel <$> h2) : zipAppend t1 t2
        zipAppend _ [] = []  -- empty phi node
        zipAppend _ _ = undefined  -- error
updatePhiNodes [] _ = []

updatePhiAndGoto :: String -> [String] -> TranspileT ()
updatePhiAndGoto k args = do
    currentLabel <- asks _currentLabel
    let newPhis = (currentLabel, ) <$> args
    modify $ \s -> s { _phiValues = updatePhi k newPhis $ _phiValues s }
    output $ SGoto (SLabel k)

nextVar :: TranspileT String
nextVar = do
    (x, xs) <- gets (fromInfiniteList . _varSupply)
    modify $ \s -> s { _varSupply = xs }
    return x

-- State
emptyState :: StateEnv
emptyState =
    StateEnv { _untranspiledJumps = []
             , _phiValues = Map.empty
             , _varSupply = [cpsToSsaVarPrefix ++ show x | x <- [(0 :: Int) ..]]
             }

updatePhi :: Label -> [(Label, String)] -> PhiMap -> PhiMap
updatePhi destLabel phiVars phiMap =
    Map.insert destLabel varList' phiMap
    where
        varList  = fromMaybe [] $ Map.lookup destLabel phiMap
        varList' = zipAppend varList phiVars
        -- TODO: Use DList
        zipAppend :: [[(Label, String)]] -> [(Label, String)] -> [[(Label, String)]]
        zipAppend (h1:t1) (h2:t2) = (h1 ++ [h2]) : zipAppend t1 t2
        zipAppend [] (h:t) = [h] : zipAppend [] t
        zipAppend l [] = l

cTypeToSSA :: CPS.CType -> SpirType
cTypeToSSA CPS.CTFloat = TFloat
cTypeToSSA CPS.CTBool = TBool
cTypeToSSA CPS.CTInt = TInt
cTypeToSSA ct@(CPS.CTFun _ _) = TFun ret args
    where
        (args, ret) = bimap (cTypeToSSA <$>) cTypeToSSA $ aggregateTypes ct
        aggregateTypes (CPS.CTFun t1' t2') = first (t1':) (aggregateTypes t2')
        aggregateTypes t = ([], t)
cTypeToSSA (CPS.CTTuple t i) = TVector (cTypeToSSA t) i
cTypeToSSA (CPS.CTStruct sName) = TStruct sName NotUniform
cTypeToSSA CPS.CTBottom = undefined

varToSSA :: CPS.Var -> Var
varToSSA (CPS.Var varName varType) = Var varName (cTypeToSSA varType)
