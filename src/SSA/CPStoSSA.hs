{-# LANGUAGE TupleSections #-}
module SSA.CPStoSSA (cpsToSSA) where

import Control.Monad.RWS
import qualified Data.Map as Map
import Data.DList (DList, toList)
import Data.Bifunctor
import Data.Maybe (fromMaybe)

import qualified CPS.AST as CPS
import SSA.AST
import Utils.DList (output)
import Utils.VarSupply (fromInfiniteList)

type Label = String
type StmtList = DList SStmt

data ContType = ReturnCont
              | RecReturnCont String
              deriving Eq
type ContTypeMap = Map.Map String ContType
type PhiMap = Map.Map Label [[(Label, String)]]  -- label: [_1 <- [(l, s)], ...]
data JumpCont = LocalCont [String] CPS.CExpr
              | ReturnJumpCont [String] CPS.CExpr

data ReaderEnv = ReaderEnv { _contTypeMap  :: ContTypeMap
                           , _currentLabel :: Label }

data StateEnv = StateEnv { _untranspiledJumps :: [(Label, JumpCont, ReaderEnv)]
                         , _phiValues :: PhiMap
                         , _varSupply :: [String] }

type TranspileT = RWS ReaderEnv StmtList StateEnv

cpsToSSA :: CPS.CFunDef -> SFnDef
cpsToSSA (CPS.CFunDef fName k args expr) = 
    SFnDef fName args' (SBlock [SGoto $ SLabel initLabel2]) labelled'
    where
        args' = SArg <$> args
        initLabel = fName ++ "_init"
        initLabel2 = initLabel ++ "2"
        initPhis = (\arg -> [(initLabel, arg)]) <$> args
        r = ReaderEnv { _contTypeMap = Map.singleton k (RecReturnCont fName)
                      , _currentLabel = initLabel }
        st = emptyState { _untranspiledJumps = [(initLabel2, ReturnJumpCont args expr, r)]
                        , _phiValues = Map.singleton initLabel2 initPhis }
        (endState, labelled) = evalJumpsToSSA st
        labelled' = updatePhiNodes labelled endState

cExprToSSA :: CPS.CExpr -> TranspileT ()
cExprToSSA (CPS.CLetVal x (CPS.CLitFloat f) cexpr) = do
    output $ SAssign x (SLitFloat f)
    cExprToSSA cexpr
cExprToSSA (CPS.CLetVal x (CPS.CLitBool b) cexpr) = do
    output $ SAssign x (SLitBool b)
    cExprToSSA cexpr
cExprToSSA (CPS.CLetVal _x (CPS.CLamCont k y c1) c2) = do
    jumps <- gets _untranspiledJumps
    closure <- ask
    let jumps' = (k, ReturnJumpCont [y] c1, closure):jumps
    modify (\ts -> ts { _untranspiledJumps = jumps' })
    local (insertContType k ReturnCont) $ cExprToSSA c2
cExprToSSA (CPS.CLetVal x (CPS.CTuple vars) cexpr) = do
    output $ SAssign x (STupleCtr vars)
    cExprToSSA cexpr
cExprToSSA (CPS.CLetProj x i t cexpr) = do
    output $ SAssign x (STupleProj i t)
    cExprToSSA cexpr
cExprToSSA (CPS.CLetCont k x c1 c2) = do
    jumps <- gets _untranspiledJumps
    closure <- ask
    let jumps' = (k, LocalCont [x] c1, closure):jumps
    modify (\ts -> ts { _untranspiledJumps = jumps' })
    cExprToSSA c2
cExprToSSA (CPS.CAppCont k x) = do
    isReturnBounded <- asks (Map.lookup k . _contTypeMap)
    case isReturnBounded of
        Just _  -> output $ SReturn (SVar x)
        Nothing -> updatePhiAndGoto k [x]
cExprToSSA (CPS.CAppFun f k args) = do
    contMap <- asks _contTypeMap
    case Map.lookup k contMap of
        Just ReturnCont -> callFnAndReturn
        Just (RecReturnCont g) ->
            if f == g
            then updatePhiAndGoto (f ++ "_init2") args -- + phiNode f_arg1 -> x
            else callFnAndReturn
        Nothing -> callFnAndJump -- + phiNode k_f_arg1 -> x
    where
        callFn = do
            v <- nextVar
            output $ SAssign v (SApp f args)
            return v
        callFnAndReturn = callFn >>= output .SReturn . SVar
        callFnAndJump = do
            v <- callFn
            updatePhiAndGoto k [v]
cExprToSSA (CPS.CLetPrim x (CPS.CBinOp op) [a, b] cexpr) = do
    output $ SAssign x (SBinOp op (SVar a) (SVar b))
    cExprToSSA cexpr
cExprToSSA (CPS.CLetPrim x (CPS.CUnOp op) [a] cexpr) = do
    output $ SAssign x (SUnOp op (SVar a))
    cExprToSSA cexpr
cExprToSSA CPS.CLetPrim {} = undefined
cExprToSSA (CPS.CIf x k1 k2) = output $ 
    SIf (SVar x) (SLabel k1) (SLabel k2)
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
        zipAppend (SPhiNode varName _:t1) (h2:t2) = 
            SPhiNode varName (bimap SLabel SArg <$> h2) : zipAppend t1 t2
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

-- Reader
insertContType :: String -> ContType -> ReaderEnv -> ReaderEnv
insertContType k contType tr = 
    tr { _contTypeMap = Map.insert k contType $ _contTypeMap tr }

-- State
emptyState :: StateEnv
emptyState = StateEnv { _untranspiledJumps = []
                      , _phiValues = Map.empty
                      , _varSupply = ["$_x_" ++ show x | x <- [(0 :: Int) ..]] }

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
