module TranspileToSSATest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import Control.Monad (unless)
import qualified Data.Map as Map

import qualified Core.AST as Core
import Core.Interpreter (eval, evalExprEnv)
import qualified Core.Interpreter as CoreInt
import Core.LambdaLifting (lambdaLiftProg)
import Core.Ops as Ops (BinOp(..))
import Core.TypeChecking (tcProgs)
-- import qualified CPS.AST as CPS
import CPS.CoreToCPS (coreToCPS)
-- import SSA.AST
import SSA.CPStoSSA (cpsToSSA)
import qualified SSA.Interpreter as SSAInt

tests :: TestTree
tests = testGroup "Transpile to SSA tests" [ coreToSSATest ]

coreToSSATest :: TestTree
coreToSSATest = testGroup "Core to SSA"
    [ testCase "Recursion g" $ liftAndTypecheck gCore >>= \x -> assertValueEq (runProc gCore (Core.App (Core.Var (Core.VarId "f" Nothing)) (Core.Lit $ Core.LFloat 7.0))) (SSAInt.run (cpsToSSA . coreToCPS <$> x) "f" [7.0] )
    , QC.testProperty "Recursion g - f(x) == f_ssa(x)" $ \(QC.Positive x) -> valueEq (runProc gCore (Core.App (Core.Var (Core.VarId "f" Nothing)) (Core.Lit $ Core.LFloat x))) (SSAInt.run (cpsToSSA . coreToCPS <$> liftAndTypecheckQC gCore) "f" [x])
    , QC.testProperty "Recursion fib – fib(x) == fib_ssa(x)" $ \(QC.Positive x) -> valueEq (runProc fibCore (Core.App (Core.Var (Core.VarId "fib" Nothing)) (Core.Lit $ Core.LFloat (fromInteger x)))) (SSAInt.run (cpsToSSA . coreToCPS <$> liftAndTypecheckQC fibCore) "fib" [fromInteger x]) ]
    where
        liftAndTypecheck core = case tcProgs (lambdaLiftProg core) of
            Left err -> assertFailure err
            Right progs -> return progs
        liftAndTypecheckQC core = case tcProgs (lambdaLiftProg core) of
            Left _ -> undefined
            Right progs -> progs
        floatId var = Core.VarId var (Just Core.TFloat)
        gType = Core.TFun Core.TFloat Core.TFloat
        helpType = Core.TFun Core.TFloat (Core.TFun Core.TFloat (Core.TFun Core.TFloat Core.TFloat))
        gCore = Core.Prog (Core.VarId "f" . Just $ Core.TFun Core.TFloat Core.TFloat) [floatId "x"] $ Core.LetFun (Core.Prog (Core.VarId "g" $ Just gType) [floatId "i"] (Core.If (Core.BinOp OpLT (Core.Var $ floatId "i") (Core.Lit $ Core.LFloat 10.0)) (Core.App (Core.Var $ Core.VarId "g" $ Just gType) (Core.BinOp OpAdd (Core.Var $ floatId "x") (Core.Var $ floatId "i"))) (Core.Var $ floatId "i"))) (Core.App (Core.Var . Core.VarId "g" $ Just gType) (Core.Lit $ Core.LFloat 0.0))
        fibCore = Core.Prog (Core.VarId "fib" . Just $ Core.TFun Core.TFloat Core.TFloat) [floatId "n"] $ Core.LetFun (Core.Prog (Core.VarId "help" $ Just helpType) [floatId "a", floatId "b", floatId "n"] (Core.If (Core.BinOp Ops.OpLT (Core.Lit $ Core.LFloat 0.0) (Core.Var $ floatId "n")) (Core.App (Core.App (Core.App (Core.Var . Core.VarId "help" $ Just helpType) (Core.Var $ floatId "b")) (Core.BinOp Ops.OpAdd (Core.Var $ floatId "a") (Core.Var $ floatId "b"))) (Core.BinOp Ops.OpSub (Core.Var $ floatId "n") (Core.Lit $ Core.LFloat 1.0))) (Core.Var $ floatId "a"))) (Core.App (Core.App (Core.App (Core.Var . Core.VarId "help" $ Just helpType) (Core.Lit $ Core.LFloat 0.0)) (Core.Lit $ Core.LFloat 1.0)) (Core.Var $ floatId "n"))

-- cpsToSSATest :: TestTree
-- cpsToSSATest = testGroup "CPS to SSA"
--     [ testCase "Recursion g" $ cpsToSSA <$> gCps @?= gSsa ]
--     where
--         gCps = [ CPS.CFunDef "f" "k" ["x"] $ CPS.CLetVal "x0" (CPS.CLitFloat 0.0) $ CPS.CAppFun "g" "k" ["x0"]
--                , CPS.CFunDef "g" "l" ["i"] $ CPS.CLetVal "x10" (CPS.CLitFloat 10.0) $ CPS.CLetPrim "res" (CPS.CBinOp OpLT) ["i", "x10"] $ CPS.CLetCont "k1" "x1" (CPS.CLetPrim "res2" (CPS.CBinOp OpAdd) ["x", "i"] $ CPS.CAppFun "g" "l" ["res2"]) $ CPS.CLetCont "k2" "x2" (CPS.CAppCont "l" "i") $ CPS.CIf "res" "k1" "k2" ]
--         gSsa = [ SFnDef "f" [SArg "x"] (SBlock [SGoto $ SLabel "f_init2"]) [ SLabelled (SLabel "f_init2") [SPhiNode "x" [(SLabel "f_init", SArg "x")]] $ SBlock [SAssign "x0" $ SLitFloat 0.0, SAssign "$_x_0" $ SApp "g" ["x0"], SReturn $ SVar "$_x_0"]]
--                , SFnDef "g" [SArg "i"] (SBlock [SGoto $ SLabel "g_init2"]) 
--                     [ SLabelled (SLabel "g_init2") [SPhiNode "i" [(SLabel "g_init", SArg "i"), (SLabel "k1", SArg "res2")]] $ SBlock [SAssign "x10" $ SLitFloat 10.0, SAssign "res" $ SBinOp Ops.OpLT (SVar "i") (SVar "x10"), SIf (SVar "res") (SLabel "k1") (SLabel "k2")]
--                     , SLabelled (SLabel "k2") [] $ SBlock [SReturn (SVar "i")]
--                     , SLabelled (SLabel "k1") [] $ SBlock [SAssign "res2" $ SBinOp Ops.OpAdd (SVar "x") (SVar "i"), SGoto $ SLabel "g_init2"]
--                     ]
--                 ]

runProc :: Core.Prog Maybe -> Core.Expr Maybe -> Either String CoreInt.Value
runProc p@(Core.Prog (Core.VarId f _) _ _) expr = do
    pVal <- eval p
    let pEnv = Map.singleton f pVal
    evalExprEnv pEnv expr

assertValueEq :: Either String CoreInt.Value 
    -> Either String SSAInt.Value 
    -> Assertion
assertValueEq l r =
    unless (valueEq l r) (assertFailure $ show l ++ " is not equal to " ++ show r)

valueEq :: Either String CoreInt.Value 
    -> Either String SSAInt.Value 
    -> Bool
valueEq (Right (CoreInt.VFloat f1)) (Right (SSAInt.VFloat f2)) = f1 == f2
valueEq _ _ = False
