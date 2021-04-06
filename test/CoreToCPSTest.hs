module CoreToCPSTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Core.AST as Core
import qualified CPS.AST as CPS
import CPS.CoreToCPS (coreToCPS)

tests :: TestTree
tests = testGroup "CoreToCPS tests" 
    [ testCase "Basic lambda" $ assertAlphaEq (coreToCPS coreLambda) cpsLambda
    , testCase "(λx. 1) 2" $ assertAlphaEq (coreToCPS coreLambda2) cpsLambda2
    , testCase "Fibonacci" $ assertAlphaEq (coreToCPS coreFib) cpsFib
    , testCase "Recursion" $ assertAlphaEq (coreToCPS coreRecursive) cpsRecursive ]
    where
        coreLambda = Core.Prog "proc" [] $ Core.Lam "x" (Core.Var "x")
        cpsLambda  = CPS.CProcLam "proc" "pCont" [] $ CPS.CLetVal "f" (CPS.CLamCont "k" "x" (CPS.CAppCont "k" "x")) (CPS.CAppCont "pCont" "f")
        coreLambda2 = Core.Prog "test"  [] $ Core.App (Core.Lam "x" (Core.Lit $ Core.LFloat 1.0)) (Core.Lit $ Core.LFloat 2.0)
        cpsLambda2  = CPS.CProcLam "test" "pCont" [] $ CPS.CLetVal "f" (CPS.CLamCont "k" "x" (CPS.CLetVal "x2" (CPS.CLitFloat 1.0) (CPS.CAppCont "k" "x2"))) (CPS.CLetVal "x1" (CPS.CLitFloat 2.0) (CPS.CAppFun "f" "pCont" ["x1"]))
        coreFib = Core.Prog "fib" ["n"] $ Core.LetRec "help" ["a", "b", "n"] (Core.If (Core.BinOp Core.OpLT (Core.Lit $ Core.LFloat 0.0) (Core.Var "n")) (Core.App (Core.App (Core.App (Core.Var "help") (Core.Var "b")) (Core.BinOp Core.OpAdd (Core.Var "a") (Core.Var "b"))) (Core.BinOp Core.OpSub (Core.Var "n") (Core.Lit $ Core.LFloat 1.0))) (Core.Var "a")) (Core.App (Core.App (Core.App (Core.Var "help") (Core.Lit $ Core.LFloat 0.0)) (Core.Lit $ Core.LFloat 1.0)) (Core.Var "n"))
        cpsFib  = CPS.CProcLam "fib" "pCont" ["n"] $ CPS.CLetFix "help" "l" ["a", "b", "n"] (CPS.CLetVal "x00" (CPS.CLitFloat 0.0) $ CPS.CLetPrim "r1" (CPS.CBinOp Core.OpLT) ["x00", "n"] (CPS.CLetCont "k1" "x1" (CPS.CLetPrim "r2" (CPS.CBinOp Core.OpAdd) ["a", "b"] (CPS.CLetVal "x01" (CPS.CLitFloat 1.0) (CPS.CLetPrim "r3" (CPS.CBinOp Core.OpSub) ["n", "x01"] (CPS.CAppFun "help" "l" ["b", "r2", "r3"])))) (CPS.CLetCont "k2" "x2" (CPS.CAppCont "l" "a") (CPS.CIf "r1" "k1" "k2")))) (CPS.CLetVal "x0" (CPS.CLitFloat 0.0) $ CPS.CLetVal "x1" (CPS.CLitFloat 1.0) $ CPS.CAppFun "help" "pCont" ["x0", "x1", "n"])
        coreRecursive = Core.Prog "proc" ["f", "limit"] (Core.LetRec "l" ["i", "c"] (Core.If (Core.BinOp Core.OpEq (Core.Var "i") (Core.Var "limit")) (Core.Var "c") (Core.If (Core.BinOp Core.OpEq (Core.App (Core.Var "f") (Core.Var "i")) (Core.Lit $ Core.LFloat 0.0)) (Core.App (Core.App (Core.Var "l") (Core.BinOp Core.OpAdd (Core.Var "i") (Core.Lit $ Core.LFloat 1.0))) (Core.BinOp Core.OpAdd (Core.Var "c") (Core.Lit $ Core.LFloat 1.0))) (Core.App (Core.App (Core.Var "l") (Core.BinOp Core.OpAdd (Core.Var "i") (Core.Lit $ Core.LFloat 1.0))) (Core.Var "c")))) (Core.App (Core.App (Core.Var "l") (Core.Lit $ Core.LFloat 0.0)) (Core.Lit $ Core.LFloat 0.0)))
        cpsRecursive = CPS.CProcLam "proc" "pCont" ["f", "limit"] $ CPS.CLetFix "l" "k" ["i", "c"] (CPS.CLetPrim "res" (CPS.CBinOp Core.OpEq) ["i", "limit"] (CPS.CLetCont "k1" "x1" (CPS.CAppCont "k" "c") (CPS.CLetCont "k2" "x2" (CPS.CLetCont "k3" "x3" (CPS.CLetVal "x0" (CPS.CLitFloat 0.0) (CPS.CLetPrim "res2" (CPS.CBinOp Core.OpEq) ["x3", "x0"] (CPS.CLetCont "k11" "x11" (CPS.CLetVal "xi1" (CPS.CLitFloat 1.0) (CPS.CLetPrim "res3" (CPS.CBinOp Core.OpAdd) ["i", "xi1"] (CPS.CLetVal "xc1" (CPS.CLitFloat 1.0) (CPS.CLetPrim "res4" (CPS.CBinOp Core.OpAdd) ["c", "xc1"] (CPS.CAppFun "l" "k" ["res3", "res4"]))))) (CPS.CLetCont "k21" "x21" (CPS.CLetVal "xj1" (CPS.CLitFloat 1.0) (CPS.CLetPrim "res5" (CPS.CBinOp Core.OpAdd) ["i", "xj1"] (CPS.CAppFun "l" "k" ["res5", "c"]))) (CPS.CIf "res2" "k11" "k21"))))) (CPS.CAppFun "f" "k3" ["i"])) (CPS.CIf "res" "k1" "k2")))) (CPS.CLetVal "x1" (CPS.CLitFloat 0.0) (CPS.CLetVal "x2" (CPS.CLitFloat 0.0) (CPS.CAppFun "l" "pCont" ["x1", "x2"])))

assertAlphaEq :: CPS.CProg -> CPS.CProg -> Assertion
assertAlphaEq x y = assertBool msg $ CPS.alphaEq x y
    where msg = "expected: " ++ show y ++ "\n but got: " ++ show x
