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
        cpsLambda2  = CPS.CProcLam "test" "pCont" [] $ CPS.CLetVal "f" (CPS.CLamCont "k" "x" (CPS.CLetVal "x2" (CPS.CLitFloat 1.0) (CPS.CAppCont "k" "x2"))) (CPS.CLetVal "x1" (CPS.CLitFloat 2.0) (CPS.CAppFun "f" "pCont" "x1"))
        coreFib = Core.Prog "fib" [] $ Core.Lam "n" (Core.LetRec "help" "a" (Core.Lam "b" (Core.Lam "n" (Core.If (Core.BinOp Core.OpLT (Core.Lit $ Core.LFloat 0.0) (Core.Var "n")) (Core.App (Core.App (Core.App (Core.Var "help") (Core.Var "b")) (Core.BinOp Core.OpAdd (Core.Var "a") (Core.Var "b"))) (Core.BinOp Core.OpSub (Core.Var "n") (Core.Lit $ Core.LFloat 1.0))) (Core.Var "a")))) (Core.App (Core.App (Core.App (Core.Var "help") (Core.Lit $ Core.LFloat 0.0)) (Core.Lit $ Core.LFloat 1.0)) (Core.Var "n")))
        cpsFib  = CPS.CProcLam "fib" "pCont" [] $ CPS.CLetVal "f" (CPS.CLamCont "k" "n" (CPS.CLetFix "help" "k2" "a" (CPS.CLetVal "f2" (CPS.CLamCont "k6" "b" $ CPS.CLetVal "f3" (CPS.CLamCont "k7" "n" $ CPS.CLetVal "x00" (CPS.CLitFloat 0.0) (CPS.CLetPrim "r1" (CPS.CBinOp Core.OpLT) ["x00", "n"] (CPS.CLetCont "k81" "x81" (CPS.CLetCont "k11" "x11" (CPS.CLetPrim "r3" (CPS.CBinOp Core.OpAdd) ["a", "b"] (CPS.CLetCont "k10" "x10" (CPS.CLetVal "x91" (CPS.CLitFloat 1.0) (CPS.CLetPrim "r2" (CPS.CBinOp Core.OpSub) ["n", "x91"] (CPS.CAppFun "x10" "k7" "r2"))) (CPS.CAppFun "x11" "k10" "r3"))) (CPS.CAppFun "help" "k11" "b")) (CPS.CLetCont "k82" "x82" (CPS.CAppCont "k7" "a") (CPS.CIf "r1" "k81" "k82"))))) (CPS.CAppCont "k6" "f3")) (CPS.CAppCont "k2" "f2")) (CPS.CLetVal "x0" (CPS.CLitFloat 0.0) (CPS.CLetCont "k5" "z" (CPS.CLetVal "x1" (CPS.CLitFloat 1.0) (CPS.CLetCont "k4" "y" (CPS.CAppFun "y" "k" "n") (CPS.CAppFun "z" "k4" "x1"))) (CPS.CAppFun "help" "k5" "x0"))))) (CPS.CAppCont "pCont" "f")
        coreRecursive = Core.Prog "proc" [] (Core.Lam "f" $ Core.Lam "limit" $ Core.LetRec "l" "i" (Core.Lam "c" (Core.If (Core.BinOp Core.OpEq (Core.Var "i") (Core.Var "limit")) (Core.Var "c") (Core.If (Core.BinOp Core.OpEq (Core.App (Core.Var "f") (Core.Var "i")) (Core.Lit $ Core.LFloat 0.0)) (Core.App (Core.App (Core.Var "l") (Core.BinOp Core.OpAdd (Core.Var "i") (Core.Lit $ Core.LFloat 1.0))) (Core.BinOp Core.OpAdd (Core.Var "c") (Core.Lit $ Core.LFloat 1.0))) (Core.App (Core.App (Core.Var "l") (Core.BinOp Core.OpAdd (Core.Var "i") (Core.Lit $ Core.LFloat 1.0))) (Core.Var "c"))))) (Core.App (Core.App (Core.Var "l") (Core.Lit $ Core.LFloat 0.0)) (Core.Lit $ Core.LFloat 0.0)))
        cpsRecursive  = CPS.CProcLam "proc" "pCont" [] (CPS.CLetVal "g" (CPS.CLamCont "j" "f" $ CPS.CLetVal "h" (CPS.CLamCont "m" "limit" $ CPS.CLetFix "l" "k1" "i" (CPS.CLetVal "f66" (CPS.CLamCont "k" "c" (CPS.CLetPrim "res" (CPS.CBinOp Core.OpEq) ["i", "limit"] (CPS.CLetCont "k1" "x1" (CPS.CAppCont "k" "c") (CPS.CLetCont "k2" "x2" (CPS.CLetCont "k7" "x7" (CPS.CLetVal "x70" (CPS.CLitFloat 0.0) (CPS.CLetPrim "res2" (CPS.CBinOp Core.OpEq) ["x7", "x70"] (CPS.CLetCont "k11" "x11" (CPS.CLetVal "xi1" (CPS.CLitFloat 1.0) (CPS.CLetPrim "resi1" (CPS.CBinOp Core.OpAdd) ["i", "xi1"] (CPS.CLetCont "ki1" "xi2" (CPS.CLetVal "x11" (CPS.CLitFloat 1.0) (CPS.CLetPrim "res3" (CPS.CBinOp Core.OpAdd) ["c", "x11"] (CPS.CAppFun "xi2" "k" "res3"))) (CPS.CAppFun "l" "ki1" "resi1")))) (CPS.CLetCont "k21" "x21" (CPS.CLetVal "xi3" (CPS.CLitFloat 1.0) (CPS.CLetPrim "resi2" (CPS.CBinOp Core.OpAdd) ["i", "xi3"] (CPS.CLetCont "ki2" "xi4" (CPS.CAppFun "xi4" "k" "c") (CPS.CAppFun "l" "ki2" "resi2")))) (CPS.CIf "res2" "k11" "k21"))))) (CPS.CAppFun "f" "k7" "i")) (CPS.CIf "res" "k1" "k2"))))) (CPS.CAppCont "k1" "f66")) (CPS.CLetVal "x0" (CPS.CLitFloat 0.0) (CPS.CLetCont "k3" "x" (CPS.CLetVal "x02" (CPS.CLitFloat 0.0) (CPS.CAppFun "x" "m" "x02")) (CPS.CAppFun "l" "k3" "x0")))) (CPS.CAppCont "j" "h")) (CPS.CAppCont "pCont" "g"))

assertAlphaEq :: CPS.CProg -> CPS.CProg -> Assertion
assertAlphaEq x y = assertBool msg $ CPS.alphaEq x y
    where msg = "expected: " ++ show y ++ "\n but got: " ++ show x
