module InterpreterTests.SSATest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Core.Ops
import SSA.AST
import SSA.Interpreter
import SPIRV.Types

tests :: TestTree
tests = testGroup "SSA tests"
    [ testCase "fib 7" $ run [gSsa] "f" [7.0] @?= Right (VFloat 14.0) ]
    where
        gSsa = SFnDef "f" TFloat [SArg $ Var "x" TFloat] (SBlock [SAssign (Var "x0" TFloat) $ SLitFloat 0.0, SGoto $ SLabel "g"]) 
            [ SLabelled (SLabel "g") [SPhiNode (Var "i" TFloat) [(SLabel "f_init", "x0"), (SLabel "k1", "res2")]] $ SBlock [SAssign (Var "x10" TFloat) $ SLitFloat 10.0, SAssign (Var "res" TFloat) $ SBinOp OpLT (SVar (Var "i" TFloat)) (SVar (Var "x10" TFloat)), SIf (SVar (Var "res" TFloat)) (SLabel "k1") (SLabel "k2")]
            , SLabelled (SLabel "k2") [] $ SBlock [SReturn (SVar (Var "i" TFloat))]
            , SLabelled (SLabel "k1") [] $ SBlock [SAssign (Var "res2" TFloat) $ SBinOp OpAdd (SVar (Var "x" TFloat)) (SVar (Var "i" TFloat)), SGoto $ SLabel "g"] ]
