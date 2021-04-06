module TranspileToSSATest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import Control.Monad (unless)
import qualified Data.Map as Map

import Core.AST (BinOp(..))
import qualified Core.AST as Core
import Core.Interpreter (eval, evalExprEnv)
import qualified Core.Interpreter as CoreInt
import qualified CPS.AST as CPS
import CPS.CoreToCPS (coreToCPS)
import SSA.AST
import SSA.CPStoSSA (cpsToSSA)
import qualified SSA.Interpreter as SSAInt

tests :: TestTree
tests = testGroup "Transpile to SSA tests" [ cpsToSSATest, coreToSSATest ]

coreToSSATest :: TestTree
coreToSSATest = testGroup "Core to SSA"
    [ testCase "Recursion g" $ assertValueEq (runProc gCore (Core.App (Core.Var "f") (Core.Lit $ Core.LFloat 7.0))) (flip SSAInt.run [7.0] . cpsToSSA . coreToCPS $ gCore)
    , QC.testProperty "Recursion g - f(x) == f_ssa(x)" $ \(QC.Positive x) -> valueEq (runProc gCore (Core.App (Core.Var "f") (Core.Lit $ Core.LFloat x))) (SSAInt.run (cpsToSSA . coreToCPS $ gCore) [x]) ]
    where
        gCore = Core.Prog "f" ["x"] $ Core.LetRec "g" ["i"] (Core.If (Core.BinOp OpLT (Core.Var "i") (Core.Lit $ Core.LFloat 10.0)) (Core.App (Core.Var "g") (Core.BinOp OpAdd (Core.Var "x") (Core.Var "i"))) (Core.Var "i")) (Core.App (Core.Var "g") (Core.Lit $ Core.LFloat 0.0))

cpsToSSATest :: TestTree
cpsToSSATest = testGroup "CPS to SSA"
    [ testCase "Recursion g" $ cpsToSSA gCps @?= gSsa ]
    where
        gCps = CPS.CProcLam "f" "k" ["x"] $ CPS.CLetFix "g" "l" ["i"] (CPS.CLetVal "x10" (CPS.CLitFloat 10.0) $ CPS.CLetPrim "res" (CPS.CBinOp OpLT) ["i", "x10"] $ CPS.CLetCont "k1" "x1" (CPS.CLetPrim "res2" (CPS.CBinOp OpAdd) ["x", "i"] $ CPS.CAppFun "g" "l" ["res2"]) $ CPS.CLetCont "k2" "x2" (CPS.CAppCont "l" "i") $ CPS.CIf "res" "k1" "k2") (CPS.CLetVal "x0" (CPS.CLitFloat 0.0) $ CPS.CAppFun "g" "k" ["x0"])
        
gSsa :: SFnDef
gSsa = SFnDef "f" [SArg "x"] (SBlock [SAssign "x0" $ SLitFloat 0.0, SGoto $ SLabel "g"]) 
    [ SLabelled (SLabel "g") [SPhiNode "i" [(SLabel "f_init", SArg "x0"), (SLabel "k1", SArg "res2")]] $ SBlock [SAssign "x10" $ SLitFloat 10.0, SAssign "res" $ SLT (SVar "i") (SVar "x10"), SIf (SVar "res") (SBlock [SGoto $ SLabel "k1"]) (SBlock [SGoto $ SLabel "k2"])]
    , SLabelled (SLabel "k2") [] $ SBlock [SReturn (SVar "i")]
    , SLabelled (SLabel "k1") [] $ SBlock [SAssign "res2" $ SAdd (SVar "x") (SVar "i"), SGoto $ SLabel "g"] ]

runProc :: Core.Prog -> Core.Expr -> Either String CoreInt.Value
runProc p@(Core.Prog f _ _) expr = do
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
