module InterpreterTests.SSATest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Core.Ops
import SSA.AST
import SSA.Interpreter

tests :: TestTree
tests = testGroup "SSA tests"
    [ testCase "fib 7" $ run gSsa [7.0] @?= Right (VFloat 14.0) ]
    where
        gSsa = SFnDef "f" [SArg "x"] (SBlock [SAssign "x0" $ SLitFloat 0.0, SGoto $ SLabel "g"]) 
            [ SLabelled (SLabel "g") [SPhiNode "i" [(SLabel "f_init", SArg "x0"), (SLabel "k1", SArg "res2")]] $ SBlock [SAssign "x10" $ SLitFloat 10.0, SAssign "res" $ SBinOp OpLT (SVar "i") (SVar "x10"), SIf (SVar "res") (SBlock [SGoto $ SLabel "k1"]) (SBlock [SGoto $ SLabel "k2"])]
            , SLabelled (SLabel "k2") [] $ SBlock [SReturn (SVar "i")]
            , SLabelled (SLabel "k1") [] $ SBlock [SAssign "res2" $ SBinOp OpAdd (SVar "x") (SVar "i"), SGoto $ SLabel "g"] ]
