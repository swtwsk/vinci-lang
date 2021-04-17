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
    , testCase "fib 7" $ assertRunProc fibProg (App (var "fib") (Lit $ LFloat 7.0)) (VFloat 13.0) ]
    where
        constLambda = LetFun (Prog (VarId "f" Nothing) [VarId "x" Nothing] (Lit $ LFloat 1.0)) (App (var "f") (Lit $ LFloat 2.0))
        tuple = Let (VarId "x" Nothing) (TupleCons [Lit $ LFloat 1.0, Lit $ LFloat 2.0]) (TupleProj 0 (var "x"))
        fibProg = Prog (VarId "fib" Nothing) [VarId "n" Nothing] (LetFun (Prog (VarId "help" Nothing) [VarId "a" Nothing, VarId "b" Nothing, VarId "n" Nothing] (If (BinOp OpLT (Lit $ LFloat 0.0) (var "n")) (App (App (App (var "help") (var "b")) (BinOp OpAdd (var "a") (var "b"))) (BinOp OpSub (var "n") (Lit $ LFloat 1.0))) (var "a"))) (App (App (App (var "help") (Lit $ LFloat 0.0)) (Lit $ LFloat 1.0)) (var "n")))

assertRunProc :: Prog Maybe -> Expr Maybe -> Value -> Assertion
assertRunProc p@(Prog (VarId f _) _ _) expr expected = do
    pVal <- case eval p of
        Left err -> assertFailure $ interpreterErrorMsg err
        Right pv -> return pv
    let pEnv = Map.singleton f pVal
    case evalExprEnv pEnv expr of
        Left err -> assertFailure $ interpreterErrorMsg err
        Right v' -> v' @?= expected
    where
        interpreterErrorMsg = \err -> "expected: " ++ show expected ++ "\n but got interpreter error instead: " ++ err

var :: String -> Expr Maybe
var x = Var $ VarId x Nothing
