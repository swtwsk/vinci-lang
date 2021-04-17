module AlphaEqTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Core.Ops (BinOp(..))
import CPS.AST

tests :: TestTree
tests = testGroup "Alpha Equivalence" 
    [ testCase "letval x = 1" $ assertAlphaEq letvalOneX letvalOneY
    -- , testCase "λx. λy. x != λy. λy. y" $ assertNotAlphaEq kCombinator nestedAllYCombinator
    -- , testCase "λy. λx. x == λy. λy. y" $ assertAlphaEq nestedCombinator nestedAllYCombinator
    -- , testCase "λx. x + 1 == λy. y + 1" $ assertAlphaEq succX succY
    , testCase "letrec f x = x + 1 in f 0" $ assertAlphaEq recWithoutCallX recWithoutCallY
    , testCase "letrec f x = f x in f 0" $ assertAlphaEq recWithCallX recWithCallY ]
    -- , testCase "λx. x != 1" $ assertNotAlphaEq iCombinator letvalOneX ]
    where
        floatVar x = Var x CTFloat
        letvalOneX = CFunDef (floatVar "proc") "k" [] $ CLetVal (floatVar "x") (CLitFloat 1.0) (CAppCont "k" (floatVar "x"))
        letvalOneY = CFunDef (floatVar "proc2") "k2" [] $ CLetVal (floatVar "y") (CLitFloat 1.0) (CAppCont "k2" (floatVar "y"))
        -- kCombinator = CFunDef "proc" "k" [] $ CLetVal "f" (CLamCont "k" (floatVar "x") (CLetVal "g" (CLamCont "k" (floatVar "y") (CAppCont "k" (floatVar "x"))) (CAppCont "k" "g"))) (CAppCont "k" "f")
        -- nestedCombinator = CFunDef "procC" "kC" [] $ CLetVal "f" (CLamCont "k" (floatVar "y") $ CLetVal "g" (CLamCont "k" (floatVar "x") (CAppCont "k" (floatVar "x"))) (CAppCont "k" "g")) (CAppCont "kC" "f")
        -- nestedAllYCombinator = CFunDef "proc2" "k2" [] $ CLetVal "f" (CLamCont "k" (floatVar "y") $ CLetVal "g" (CLamCont "k" (floatVar "y") (CAppCont "k" (floatVar "y"))) (CAppCont "k" "g")) (CAppCont "k2" "f")
        -- succX = CFunDef "procx" "kx" [] $ CLetVal "f" (CLamCont "k" (floatVar "x") (CLetVal "x1" (CLitFloat 1.0) (CLetPrim (floatVar "r1") (CBinOp OpAdd) [(floatVar "x"), "x1"] (CAppCont "k" (floatVar "r1"))))) (CAppCont "kx" "f")
        -- succY = CFunDef "procy" "ky" [] $ CLetVal "f" (CLamCont "k" (floatVar "y") (CLetVal "x1" (CLitFloat 1.0) (CLetPrim (floatVar "r1") (CBinOp OpAdd) [(floatVar "y"), "x1"] (CAppCont "k" (floatVar "r1"))))) (CAppCont "ky" "f")
        recWithoutCallX = CFunDef (floatVar "procrx") "krx" [] $ CLetFun (CFunDef (Var "f" $ CTFun CTFloat CTFloat) "k" [floatVar "x"] (CLetVal (floatVar "x1") (CLitFloat 1.0) (CLetPrim (floatVar "r1") (CBinOp OpAdd) [floatVar "x", floatVar "x1"] (CAppCont "k" (floatVar "r1"))))) (CLetVal (floatVar "x0") (CLitFloat 0.0) (CLetCont "k" (floatVar "x") (CAppCont "krx" (floatVar "x")) (CAppFun (Var "f" $ CTFun CTFloat CTFloat) "k" [floatVar "x0"])))
        recWithoutCallY = CFunDef (floatVar "procry") "kry" [] $ CLetFun (CFunDef (Var "g" $ CTFun CTFloat CTFloat) "h" [floatVar "y"] (CLetVal (floatVar "x1") (CLitFloat 1.0) (CLetPrim (floatVar "r1") (CBinOp OpAdd) [floatVar "y", floatVar "x1"] (CAppCont "h" (floatVar "r1"))))) (CLetVal (floatVar "x0") (CLitFloat 0.0) (CLetCont "k" (floatVar "x") (CAppCont "kry" (floatVar "x")) (CAppFun (Var "g" $ CTFun CTFloat CTFloat) "k" [floatVar "x0"])))
        recWithCallX = CFunDef (floatVar "procrx") "kx" [] $ CLetFun (CFunDef (Var "f" $ CTFun CTFloat CTFloat) "k" [floatVar "x"] (CLetCont "k2" (floatVar "x") (CAppCont "k" (floatVar "x")) (CAppFun (Var "f" $ CTFun CTFloat CTFloat) "k2" [floatVar "x"]))) (CLetVal (floatVar "x0") (CLitFloat 0.0) (CLetCont "k" (floatVar "x") (CAppCont "kx" (floatVar "x")) (CAppFun (Var "f" $ CTFun CTFloat CTFloat) "k" [floatVar "x0"])))
        recWithCallY = CFunDef (floatVar "procry") "ky" [] $ CLetFun (CFunDef (Var "g" $ CTFun CTFloat CTFloat) "h" [floatVar "y"] (CLetCont "k2" (floatVar "x") (CAppCont "h" (floatVar "x")) (CAppFun (Var "g" $ CTFun CTFloat CTFloat) "k2" [floatVar "y"]))) (CLetVal (floatVar "x0") (CLitFloat 0.0) (CLetCont "k" (floatVar "x") (CAppCont "ky" (floatVar "x")) (CAppFun (Var "g" $ CTFun CTFloat CTFloat) "k" [floatVar "x0"])))
        -- iCombinator = CFunDef "proci" "ki" [] $ CLetVal "f" (CLamCont "k" (floatVar "x") $ CAppCont "k" (floatVar "x")) (CAppCont "ki" "f")

assertAlphaEq :: (AlphaEq a, Show a) => a -> a -> Assertion
assertAlphaEq x y = assertBool msg $ alphaEq x y
    where msg = show x ++ " is not α-equivalent to " ++ show y

-- assertNotAlphaEq :: (AlphaEq a, Show a) => a -> a -> Assertion
-- assertNotAlphaEq x y = assertBool msg $ not (alphaEq x y)
--     where msg = show x ++ " is α-equivalent to " ++ show y ++ " but it should not be"
