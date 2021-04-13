module AlphaEqTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Core.Ops (BinOp(..))
import CPS.AST

tests :: TestTree
tests = testGroup "Alpha Equivalence" 
    [ testCase "letval x = 1" $ assertAlphaEq letvalOneX letvalOneY
    , testCase "λx. λy. x != λy. λy. y" $ assertNotAlphaEq kCombinator nestedAllYCombinator
    , testCase "λy. λx. x == λy. λy. y" $ assertAlphaEq nestedCombinator nestedAllYCombinator
    , testCase "λx. x + 1 == λy. y + 1" $ assertAlphaEq succX succY
    , testCase "letrec f x = x + 1 in f 0" $ assertAlphaEq recWithoutCallX recWithoutCallY
    , testCase "letrec f x = f x in f 0" $ assertAlphaEq recWithCallX recWithCallY
    , testCase "λx. x != 1" $ assertNotAlphaEq iCombinator letvalOneX ]
    where
        letvalOneX = CFunDef "proc" "k" [] $ CLetVal "x" (CLitFloat 1.0) (CAppCont "k" "x")
        letvalOneY = CFunDef "proc2" "k2" [] $ CLetVal "y" (CLitFloat 1.0) (CAppCont "k2" "y")
        kCombinator = CFunDef "proc" "k" [] $ CLetVal "f" (CLamCont "k" "x" (CLetVal "g" (CLamCont "k" "y" (CAppCont "k" "x")) (CAppCont "k" "g"))) (CAppCont "k" "f")
        nestedCombinator = CFunDef "procC" "kC" [] $ CLetVal "f" (CLamCont "k" "y" $ CLetVal "g" (CLamCont "k" "x" (CAppCont "k" "x")) (CAppCont "k" "g")) (CAppCont "kC" "f")
        nestedAllYCombinator = CFunDef "proc2" "k2" [] $ CLetVal "f" (CLamCont "k" "y" $ CLetVal "g" (CLamCont "k" "y" (CAppCont "k" "y")) (CAppCont "k" "g")) (CAppCont "k2" "f")
        succX = CFunDef "procx" "kx" [] $ CLetVal "f" (CLamCont "k" "x" (CLetVal "x1" (CLitFloat 1.0) (CLetPrim "r1" (CBinOp OpAdd) ["x", "x1"] (CAppCont "k" "r1")))) (CAppCont "kx" "f")
        succY = CFunDef "procy" "ky" [] $ CLetVal "f" (CLamCont "k" "y" (CLetVal "x1" (CLitFloat 1.0) (CLetPrim "r1" (CBinOp OpAdd) ["y", "x1"] (CAppCont "k" "r1")))) (CAppCont "ky" "f")
        recWithoutCallX = CFunDef "procrx" "krx" [] $ CLetFix "f" "k" ["x"] (CLetVal "x1" (CLitFloat 1.0) (CLetPrim "r1" (CBinOp OpAdd) ["x", "x1"] (CAppCont "k" "r1"))) (CLetVal "x0" (CLitFloat 0.0) (CLetCont "k" "x" (CAppCont "krx" "x") (CAppFun "f" "k" ["x0"])))
        recWithoutCallY = CFunDef "procry" "kry" [] $ CLetFix "g" "h" ["y"] (CLetVal "x1" (CLitFloat 1.0) (CLetPrim "r1" (CBinOp OpAdd) ["y", "x1"] (CAppCont "h" "r1"))) (CLetVal "x0" (CLitFloat 0.0) (CLetCont "k" "x" (CAppCont "kry" "x") (CAppFun "g" "k" ["x0"])))
        recWithCallX = CFunDef "procx" "kx" [] $ CLetFix "f" "k" ["x"] (CLetCont "k2" "x" (CAppCont "k" "x") (CAppFun "f" "k2" ["x"])) (CLetVal "x0" (CLitFloat 0.0) (CLetCont "k" "x" (CAppCont "kx" "x") (CAppFun "f" "k" ["x0"])))
        recWithCallY = CFunDef "procy" "ky" [] $ CLetFix "g" "h" ["y"] (CLetCont "k2" "x" (CAppCont "h" "x") (CAppFun "g" "k2" ["y"])) (CLetVal "x0" (CLitFloat 0.0) (CLetCont "k" "x" (CAppCont "ky" "x") (CAppFun "g" "k" ["x0"])))
        iCombinator = CFunDef "proci" "ki" [] $ CLetVal "f" (CLamCont "k" "x" $ CAppCont "k" "x") (CAppCont "ki" "f")

assertAlphaEq :: (AlphaEq a, Show a) => a -> a -> Assertion
assertAlphaEq x y = assertBool msg $ alphaEq x y
    where msg = show x ++ " is not α-equivalent to " ++ show y

assertNotAlphaEq :: (AlphaEq a, Show a) => a -> a -> Assertion
assertNotAlphaEq x y = assertBool msg $ not (alphaEq x y)
    where msg = show x ++ " is α-equivalent to " ++ show y ++ " but it should not be"
