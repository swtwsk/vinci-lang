module CoreToCPSTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Core.AST as Core
import Core.Ops as Ops
import qualified CPS.AST as CPS
import CPS.CoreToCPS (coreToCPS)

tests :: TestTree
tests = testGroup "CoreToCPS tests" 
    [ testCase "Basic lambda" $ assertAlphaEq (coreToCPS coreLambda) cpsLambda
    , testCase "Tuple const and deconst" $ assertAlphaEq (coreToCPS coreTuple) cpsTuple
    , testCase "Fibonacci" $ assertAlphaEq (coreToCPS coreFib) cpsFib
    , testCase "Recursion" $ assertAlphaEq (coreToCPS coreRecursive) cpsRecursive ]
    where
        coreLambda = Core.Prog "test" [] $ Core.LetFun (Core.Prog "f" ["x"] (Core.Lit $ Core.LFloat 1.0)) (Core.App (Core.Var "f") (Core.Lit $ Core.LFloat 2.0))
        cpsLambda  = CPS.CFunDef "test" "pCont" [] $ CPS.CLetFun (CPS.CFunDef "f" "k2" ["x"] $ CPS.CLetVal "x1" (CPS.CLitFloat 1.0) $ CPS.CAppCont "k2" "x1") (CPS.CLetVal "x2" (CPS.CLitFloat 2.0) $ CPS.CAppFun "f" "pCont" ["x2"])
        coreTuple = Core.Prog "test" [] $ Core.Let "x" (Core.TupleCons [Core.Lit $ Core.LFloat 1.0, Core.Lit $ Core.LFloat 2.0]) (Core.TupleProj 0 (Core.Var "x"))
        cpsTuple = CPS.CFunDef "test" "k" [] $ CPS.CLetCont "j" "x" (CPS.CLetProj "x2" 0 "x" $ CPS.CAppCont "k" "x2") (CPS.CLetVal "t0" (CPS.CLitFloat 1.0) (CPS.CLetVal "t1" (CPS.CLitFloat 2.0) (CPS.CLetVal "t" (CPS.CTuple ["t0", "t1"]) $ CPS.CAppCont "j" "t")))
        coreFib = Core.Prog "fib" ["n"] $ Core.LetFun (Core.Prog "help" ["a", "b", "n"] (Core.If (Core.BinOp Ops.OpLT (Core.Lit $ Core.LFloat 0.0) (Core.Var "n")) (Core.App (Core.App (Core.App (Core.Var "help") (Core.Var "b")) (Core.BinOp Ops.OpAdd (Core.Var "a") (Core.Var "b"))) (Core.BinOp Ops.OpSub (Core.Var "n") (Core.Lit $ Core.LFloat 1.0))) (Core.Var "a"))) (Core.App (Core.App (Core.App (Core.Var "help") (Core.Lit $ Core.LFloat 0.0)) (Core.Lit $ Core.LFloat 1.0)) (Core.Var "n"))
        cpsFib  = CPS.CFunDef "fib" "pCont" ["n"] $ CPS.CLetFun (CPS.CFunDef "help" "l" ["a", "b", "n"] (CPS.CLetVal "x00" (CPS.CLitFloat 0.0) $ CPS.CLetPrim "r1" (CPS.CBinOp Ops.OpLT) ["x00", "n"] (CPS.CLetCont "k1" "x1" (CPS.CLetPrim "r2" (CPS.CBinOp Ops.OpAdd) ["a", "b"] (CPS.CLetVal "x01" (CPS.CLitFloat 1.0) (CPS.CLetPrim "r3" (CPS.CBinOp Ops.OpSub) ["n", "x01"] (CPS.CAppFun "help" "l" ["b", "r2", "r3"])))) (CPS.CLetCont "k2" "x2" (CPS.CAppCont "l" "a") (CPS.CIf "r1" "k1" "k2"))))) (CPS.CLetVal "x0" (CPS.CLitFloat 0.0) $ CPS.CLetVal "x1" (CPS.CLitFloat 1.0) $ CPS.CAppFun "help" "pCont" ["x0", "x1", "n"])
        coreRecursive = Core.Prog  "proc" ["f", "limit"] (Core.LetFun (Core.Prog "l" ["i", "c"] (Core.If (Core.BinOp Ops.OpEq (Core.Var "i") (Core.Var "limit")) (Core.Var "c") (Core.If (Core.BinOp Ops.OpEq (Core.App (Core.Var "f") (Core.Var "i")) (Core.Lit $ Core.LFloat 0.0)) (Core.App (Core.App (Core.Var "l") (Core.BinOp Ops.OpAdd (Core.Var "i") (Core.Lit $ Core.LFloat 1.0))) (Core.BinOp Ops.OpAdd (Core.Var "c") (Core.Lit $ Core.LFloat 1.0))) (Core.App (Core.App (Core.Var "l") (Core.BinOp Ops.OpAdd (Core.Var "i") (Core.Lit $ Core.LFloat 1.0))) (Core.Var "c"))))) (Core.App (Core.App (Core.Var "l") (Core.Lit $ Core.LFloat 0.0)) (Core.Lit $ Core.LFloat 0.0)))
        cpsRecursive = CPS.CFunDef "proc" "pCont" ["f", "limit"] $ CPS.CLetFun (CPS.CFunDef "l" "k" ["i", "c"] (CPS.CLetPrim "res" (CPS.CBinOp Ops.OpEq) ["i", "limit"] (CPS.CLetCont "k1" "x1" (CPS.CAppCont "k" "c") (CPS.CLetCont "k2" "x2" (CPS.CLetCont "k3" "x3" (CPS.CLetVal "x0" (CPS.CLitFloat 0.0) (CPS.CLetPrim "res2" (CPS.CBinOp Ops.OpEq) ["x3", "x0"] (CPS.CLetCont "k11" "x11" (CPS.CLetVal "xi1" (CPS.CLitFloat 1.0) (CPS.CLetPrim "res3" (CPS.CBinOp Ops.OpAdd) ["i", "xi1"] (CPS.CLetVal "xc1" (CPS.CLitFloat 1.0) (CPS.CLetPrim "res4" (CPS.CBinOp Ops.OpAdd) ["c", "xc1"] (CPS.CAppFun "l" "k" ["res3", "res4"]))))) (CPS.CLetCont "k21" "x21" (CPS.CLetVal "xj1" (CPS.CLitFloat 1.0) (CPS.CLetPrim "res5" (CPS.CBinOp Ops.OpAdd) ["i", "xj1"] (CPS.CAppFun "l" "k" ["res5", "c"]))) (CPS.CIf "res2" "k11" "k21"))))) (CPS.CAppFun "f" "k3" ["i"])) (CPS.CIf "res" "k1" "k2"))))) (CPS.CLetVal "x1" (CPS.CLitFloat 0.0) (CPS.CLetVal "x2" (CPS.CLitFloat 0.0) (CPS.CAppFun "l" "pCont" ["x1", "x2"])))

assertAlphaEq :: CPS.CFunDef -> CPS.CFunDef -> Assertion
assertAlphaEq x y = assertBool msg $ CPS.alphaEq x y
    where msg = "expected: " ++ show y ++ "\n but got: " ++ show x
