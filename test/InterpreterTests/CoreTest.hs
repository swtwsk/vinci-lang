module InterpreterTests.CoreTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Map as Map

import Core.AST
import Core.Interpreter
import Core.Ops

tests :: TestTree
tests = testGroup "Core tests"
    [ testCase "(λx. 1) 2" $ evalExpr constLambda @?= Right (VFloat 1.0)
    , testCase "let x = (1.0, 2.0) in #0 x" $ evalExpr tuple @?= Right (VFloat 1.0)
    , testCase "fib 7" $ assertRunProc fibProg (App (Var "fib") (Lit $ LFloat 7.0)) (VFloat 13.0) ]
    where
        constLambda = LetFun (Prog NonRec "f" ["x"] (Lit $ LFloat 1.0)) (App (Var "f") (Lit $ LFloat 2.0))
        tuple = Let "x" (TupleCons [Lit $ LFloat 1.0, Lit $ LFloat 2.0]) (TupleProj 0 (Var "x"))
        fibProg = Prog NonRec "fib" ["n"] (LetFun (Prog Rec "help" ["a", "b", "n"] (If (BinOp OpLT (Lit $ LFloat 0.0) (Var "n")) (App (App (App (Var "help") (Var "b")) (BinOp OpAdd (Var "a") (Var "b"))) (BinOp OpSub (Var "n") (Lit $ LFloat 1.0))) (Var "a"))) (App (App (App (Var "help") (Lit $ LFloat 0.0)) (Lit $ LFloat 1.0)) (Var "n")))

assertRunProc :: Prog -> Expr -> Value -> Assertion
assertRunProc p@(Prog _ f _ _) expr expected = do
    pVal <- case eval p of
        Left err -> assertFailure $ interpreterErrorMsg err
        Right pv -> return pv
    let pEnv = Map.singleton f pVal
    case evalExprEnv pEnv expr of
        Left err -> assertFailure $ interpreterErrorMsg err
        Right v' -> v' @?= expected
    where
        interpreterErrorMsg = \err -> "expected: " ++ show expected ++ "\n but got interpreter error instead: " ++ err
