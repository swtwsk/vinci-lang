module SSA.Interpreter where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.List (intercalate)
import qualified Data.Map as Map

import Core.Ops
import SSA.AST

type Err = String
type FunctionsMap = Map.Map String SFnDef
data StateEnv = StateEnv { _values    :: Map.Map String Value
                         , _labelled  :: Map.Map SLabel SLabelledBlock
                         , _currentLabel :: SLabel }
type SSAM = ReaderT FunctionsMap (StateT StateEnv (Except Err))

data Value = VFloat Double
           | VBool Bool
           | VTuple [Value]
           deriving Eq

run :: [SFnDef] -> String -> [Double] -> Either Err Value
run defs mainName args = case run'' of
    Right (Just v) -> Right v
    Right Nothing -> Left "No value returned"
    Left err -> Left err
    where
        run'' = runExcept (evalStateT (runReaderT (run' main args') funs) st)
        funs  = Map.fromList $ fmap (\f@(SFnDef fName _ _ _ _) -> (fName, f)) defs
        main  = funs Map.! mainName
        args' = VFloat <$> args
        st = StateEnv { _values = Map.empty
                      , _labelled = Map.empty
                      , _currentLabel = SLabel $ mainName ++ "_init" }

run' :: SFnDef -> [Value] -> SSAM (Maybe Value)
run' (SFnDef fName _ fArgs block labelled) args = put st >> runBlock block
    where
        valuesList = (\(SArg (Var arg _), f) -> (arg, f)) <$> zip fArgs args
        labelled' = (\lb@(SLabelled l _ _) -> (l, lb)) <$> labelled
        st = StateEnv { _values = Map.fromList valuesList
                      , _labelled = Map.fromList labelled'
                      , _currentLabel = SLabel $ fName ++ "_init" }

runLabelled :: SLabelledBlock -> SLabel -> SSAM (Maybe Value)
runLabelled (SLabelled l phis block) lastLabel = do
    mapM_ runStmt phiAssignments
    modify $ \st -> st { _currentLabel = l }
    runBlock block
    where
        phiAssignments = map (\(a@(Var _ t), b) -> SAssign a (SVar (Var b t))) phis'
        phis' = second getArg . phiToPair <$> phis
        phiToPair (SPhiNode v labelArgPair) = (v, labelArgPair)
        getArg = head . map snd . filter (\(l', _) -> l' == lastLabel)

runBlock :: SBlock -> SSAM (Maybe Value)
runBlock (SBlock stmts) = last <$> mapM runStmt stmts

runStmt :: SStmt -> SSAM (Maybe Value)
runStmt (SAssign (Var v _) e) = do
    e' <- runExpr e
    st <- get
    let values  = _values st
        values' = Map.insert v e' values
    put $ st { _values = values' }
    return Nothing
runStmt (SGoto l) = do
    allLabelled <- gets _labelled
    currentLabel <- gets _currentLabel
    let labelled = Map.lookup l allLabelled
        err = "Cannot find block " ++ show l
    maybe (throwError err) (`runLabelled` currentLabel) labelled
runStmt (SReturn e) = pure <$> runExpr e
runStmt (SIf _ cond b1 b2) = do
    cond' <- runExpr cond
    case cond' of
        VBool True -> runStmt (SGoto b1)
        VBool False -> runStmt (SGoto b2)
        _ -> throwError $ show cond ++ " is not a proper condition"

runExpr :: SExpr -> SSAM Value
runExpr (SVar (Var var _)) = do
    values <- gets _values
    let value = Map.lookup var values
    maybe (throwError $ "Unbound variable " ++ var) return value
runExpr (SApp (Var f _) args) = do
    oldState <- get
    fDef <- asks (Map.! f)
    let argsVal = (_values oldState Map.!) . _varName <$> args
    val  <- run' fDef argsVal
    put oldState
    maybe (throwError $ f ++ " hasn't returned anything") return val
runExpr (STupleCtr vars) = VTuple <$> forM vars (runExpr . SVar)
runExpr (STupleProj i (Var v _)) = do
    values <- gets _values
    case Map.lookup v values of
        Just (VTuple vals) -> return $ vals !! i
        Just val -> throwError $ "Expected tuple, got " ++ show val ++ " instead"
        Nothing -> throwError $ "Unbound variable " ++ v
runExpr (SBinOp op e1 e2) = do
    e1' <- runExpr e1
    e2' <- runExpr e2
    case (op, e1', e2') of
        (OpAdd, VFloat f1, VFloat f2) -> return . VFloat $ f1 + f2
        (OpSub, VFloat f1, VFloat f2) -> return . VFloat $ f1 - f2
        (OpMul, VFloat f1, VFloat f2) -> return . VFloat $ f1 * f2
        (OpDiv, VFloat _, VFloat 0.0) -> throwError "Division by zero"
        (OpDiv, VFloat f1, VFloat f2) -> return . VFloat $ f1 / f2
        (OpAnd, VBool b1, VBool b2)   -> return . VBool $ b1 && b2
        (OpOr, VBool b1, VBool b2)    -> return . VBool $ b1 || b2
        (OpEq, VFloat f1, VFloat f2)  -> return . VBool $ f1 == f2
        (OpEq, VBool b1, VBool b2)    -> return . VBool $ b1 == b2
        (OpLT, VFloat f1, VFloat f2)  -> return . VBool $ f1 < f2
        _ -> throwError $ "Cannot do " ++ show op ++ " on " ++ show e1' 
                ++ " and " ++ show e2'
runExpr (SUnOp op e) = do
    e' <- runExpr e
    case (op, e') of
        (OpNeg, VFloat f) -> return . VFloat $ -f
        (OpNot, VBool b) -> return . VBool $ not b
        _ -> throwError $ "Cannot do " ++ show op ++ " on " ++ show e'
runExpr (SLitFloat f) = return $ VFloat f
runExpr (SLitBool b) = return $ VBool b

instance Show Value where
    show (VFloat f) = show f
    show (VBool b)  = show b
    show (VTuple vals) = "(" ++ intercalate ", " (show <$> vals) ++ ")"
