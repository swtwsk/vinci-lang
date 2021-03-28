module AlphaEqTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import CPS.AST
import Core.AST (BinOp(..))

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
        letvalOneX = CLetVal "x" (CLitFloat 1.0) (CExit "x")
        letvalOneY = CLetVal "y" (CLitFloat 1.0) (CExit "y")
        kCombinator = CLetVal "f" (CLamCont "k" "x" (CLetVal "g" (CLamCont "k" "y" (CAppCont "k" "x")) (CAppCont "k" "g"))) (CExit "f")
        nestedCombinator = CLetVal "f" (CLamCont "k" "y" $ CLetVal "g" (CLamCont "k" "x" (CAppCont "k" "x")) (CAppCont "k" "g")) (CExit "f")
        nestedAllYCombinator = CLetVal "f" (CLamCont "k" "y" $ CLetVal "g" (CLamCont "k" "y" (CAppCont "k" "y")) (CAppCont "k" "g")) (CExit "f")
        succX = CLetVal "f" (CLamCont "k" "x" (CLetVal "x1" (CLitFloat 1.0) (CLetPrim "r1" (CBinOp OpAdd) ["x", "x1"] (CAppCont "k" "r1")))) (CExit "f")
        succY = CLetVal "f" (CLamCont "k" "y" (CLetVal "x1" (CLitFloat 1.0) (CLetPrim "r1" (CBinOp OpAdd) ["y", "x1"] (CAppCont "k" "r1")))) (CExit "f")
        recWithoutCallX = CLetFix "f" "k" "x" (CLetVal "x1" (CLitFloat 1.0) (CLetPrim "r1" (CBinOp OpAdd) ["x", "x1"] (CAppCont "k" "r1"))) (CLetVal "x0" (CLitFloat 0.0) (CLetCont "k" "x" (CExit "x") (CAppFun "f" "k" "x0")))
        recWithoutCallY = CLetFix "g" "h" "y" (CLetVal "x1" (CLitFloat 1.0) (CLetPrim "r1" (CBinOp OpAdd) ["y", "x1"] (CAppCont "h" "r1"))) (CLetVal "x0" (CLitFloat 0.0) (CLetCont "k" "x" (CExit "x") (CAppFun "g" "k" "x0")))
        recWithCallX = CLetFix "f" "k" "x" (CLetCont "k2" "x" (CAppCont "k" "x") (CAppFun "f" "k2" "x")) (CLetVal "x0" (CLitFloat 0.0) (CLetCont "k" "x" (CExit "x") (CAppFun "f" "k" "x0")))
        recWithCallY = CLetFix "g" "h" "y" (CLetCont "k2" "x" (CAppCont "h" "x") (CAppFun "g" "k2" "y")) (CLetVal "x0" (CLitFloat 0.0) (CLetCont "k" "x" (CExit "x") (CAppFun "g" "k" "x0")))
        iCombinator = CLetVal "f" (CLamCont "k" "x" $ CAppCont "k" "x") (CExit "f")

assertAlphaEq :: (AlphaEq a, Show a) => a -> a -> Assertion
assertAlphaEq x y = assertBool msg $ alphaEq x y
    where msg = show x ++ " is not α-equivalent to " ++ show y

assertNotAlphaEq :: (AlphaEq a, Show a) => a -> a -> Assertion
assertNotAlphaEq x y = assertBool msg $ not (alphaEq x y)
    where msg = show x ++ " is α-equivalent to " ++ show y ++ " but it should not be"
