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
import qualified CPS.AST as CPS
import CPS.CoreToCPS (coreToCPS)
import SSA.AST
import SSA.CPStoSSA (cpsToSSA)
import qualified SSA.Interpreter as SSAInt

tests :: TestTree
tests = testGroup "Transpile to SSA tests" [ cpsToSSATest, coreToSSATest ]

coreToSSATest :: TestTree
coreToSSATest = testGroup "Core to SSA"
    [ testCase "Recursion g" $ assertValueEq (runProc gCore (Core.App (Core.Var "f") (Core.Lit $ Core.LFloat 7.0))) (SSAInt.run (cpsToSSA . coreToCPS <$> lambdaLiftProg gCore) "f" [7.0] )
    , QC.testProperty "Recursion g - f(x) == f_ssa(x)" $ \(QC.Positive x) -> valueEq (runProc gCore (Core.App (Core.Var "f") (Core.Lit $ Core.LFloat x))) (SSAInt.run (cpsToSSA . coreToCPS <$> lambdaLiftProg gCore) "f" [x])
    , QC.testProperty "Recursion fib – fib(x) == fib_ssa(x)" $ \(QC.Positive x) -> valueEq (runProc fibCore (Core.App (Core.Var "fib") (Core.Lit $ Core.LFloat (fromInteger x)))) (SSAInt.run (cpsToSSA . coreToCPS <$> lambdaLiftProg fibCore) "fib" [fromInteger x]) ]
    where
        gCore = Core.Prog Core.NonRec "f" ["x"] $ Core.LetFun (Core.Prog Core.Rec "g" ["i"] (Core.If (Core.BinOp OpLT (Core.Var "i") (Core.Lit $ Core.LFloat 10.0)) (Core.App (Core.Var "g") (Core.BinOp OpAdd (Core.Var "x") (Core.Var "i"))) (Core.Var "i"))) (Core.App (Core.Var "g") (Core.Lit $ Core.LFloat 0.0))
        fibCore = Core.Prog Core.NonRec "fib" ["n"] $ Core.LetFun (Core.Prog Core.Rec "help" ["a", "b", "n"] (Core.If (Core.BinOp Ops.OpLT (Core.Lit $ Core.LFloat 0.0) (Core.Var "n")) (Core.App (Core.App (Core.App (Core.Var "help") (Core.Var "b")) (Core.BinOp Ops.OpAdd (Core.Var "a") (Core.Var "b"))) (Core.BinOp Ops.OpSub (Core.Var "n") (Core.Lit $ Core.LFloat 1.0))) (Core.Var "a"))) (Core.App (Core.App (Core.App (Core.Var "help") (Core.Lit $ Core.LFloat 0.0)) (Core.Lit $ Core.LFloat 1.0)) (Core.Var "n"))

cpsToSSATest :: TestTree
cpsToSSATest = testGroup "CPS to SSA"
    [ testCase "Recursion g" $ cpsToSSA <$> gCps @?= gSsa ]
    where
        gCps = [ CPS.CFunDef "f" "k" ["x"] $ CPS.CLetVal "x0" (CPS.CLitFloat 0.0) $ CPS.CAppFun "g" "k" ["x0"]
               , CPS.CFunDef "g" "l" ["i"] $ CPS.CLetVal "x10" (CPS.CLitFloat 10.0) $ CPS.CLetPrim "res" (CPS.CBinOp OpLT) ["i", "x10"] $ CPS.CLetCont "k1" "x1" (CPS.CLetPrim "res2" (CPS.CBinOp OpAdd) ["x", "i"] $ CPS.CAppFun "g" "l" ["res2"]) $ CPS.CLetCont "k2" "x2" (CPS.CAppCont "l" "i") $ CPS.CIf "res" "k1" "k2" ]
        gSsa = [ SFnDef "f" [SArg "x"] (SBlock [SGoto $ SLabel "f_init2"]) [ SLabelled (SLabel "f_init2") [SPhiNode "x" [(SLabel "f_init", SArg "x")]] $ SBlock [SAssign "x0" $ SLitFloat 0.0, SAssign "$_x_0" $ SApp "g" ["x0"], SReturn $ SVar "$_x_0"]]
               , SFnDef "g" [SArg "i"] (SBlock [SGoto $ SLabel "g_init2"]) 
                    [ SLabelled (SLabel "g_init2") [SPhiNode "i" [(SLabel "g_init", SArg "i"), (SLabel "k1", SArg "res2")]] $ SBlock [SAssign "x10" $ SLitFloat 10.0, SAssign "res" $ SBinOp Ops.OpLT (SVar "i") (SVar "x10"), SIf (SVar "res") (SLabel "k1") (SLabel "k2")]
                    , SLabelled (SLabel "k2") [] $ SBlock [SReturn (SVar "i")]
                    , SLabelled (SLabel "k1") [] $ SBlock [SAssign "res2" $ SBinOp Ops.OpAdd (SVar "x") (SVar "i"), SGoto $ SLabel "g_init2"]
                    ]
                ]

runProc :: Core.Prog -> Core.Expr -> Either String CoreInt.Value
runProc p@(Core.Prog _ f _ _) expr = do
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
