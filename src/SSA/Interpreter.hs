module SSA.Interpreter where

import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import qualified Data.Map as Map

import Core.Ops
import SSA.AST

type Err = String
data StateEnv = StateEnv { _values   :: Map.Map String Value
                         , _labelled :: Map.Map SLabel SLabelledBlock
                         , _currentLabel :: SLabel }
type SSAM = StateT StateEnv (Except Err)

data Value = VFloat Double
           | VBool Bool
        --    | VFun [String] 
           deriving Eq

run :: SFnDef -> [Double] -> Either Err Value
run (SFnDef fName fArgs block labelled) args = case run' of
    Right (Just v) -> Right v
    Right Nothing -> Left "No value returned"
    Left err -> Left err
    where
        run' = runExcept (evalStateT (runBlock block) st)
        valuesList = (\(SArg arg, f) -> (arg, VFloat f)) <$> zip fArgs args
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
        phiAssignments = map (\(a, b) -> SAssign a (SVar b)) phis'
        phis' = second ((\(SArg a) -> a) . getArg) . phiToPair <$> phis
        phiToPair (SPhiNode v labelArgPair) = (v, labelArgPair)
        getArg = head . map snd . filter (\(l', _) -> l' == lastLabel)

runBlock :: SBlock -> SSAM (Maybe Value)
runBlock (SBlock stmts) = last <$> mapM runStmt stmts

runStmt :: SStmt -> SSAM (Maybe Value)
runStmt (SAssign v e) = do
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
runStmt (SIf cond b1 b2) = do
    cond' <- runExpr cond
    case cond' of
        VBool True -> runStmt (SGoto b1)
        VBool False -> runStmt (SGoto b2)
        _ -> throwError $ show cond ++ " is not a proper condition"

runExpr :: SExpr -> SSAM Value
runExpr (SVar var) = do
    values <- gets _values
    let value = Map.lookup var values
    maybe (throwError $ "Unbound variable " ++ var) return value
runExpr (SApp _f _args) = undefined
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

instance Show Value where
    show (VFloat f) = show f
    show (VBool b)  = show b
