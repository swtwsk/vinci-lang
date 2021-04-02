module TranspileToSSATest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Core.AST (BinOp(..))
import qualified CPS.AST as CPS
import SSA.AST
import SSA.CPStoSSA (cpsToSSA)

tests :: TestTree
tests = testGroup "Transpile to SSA tests" [ cpsToSSATest ]

cpsToSSATest :: TestTree
cpsToSSATest = testGroup "CPS to SSA"
    [ testCase "Recursion g" $ cpsToSSA gCps @?= gSsa ]
    where
        gCps = CPS.CProcLam "f" "k" ["x"] $ CPS.CLetFix "g" "l" "i" (CPS.CLetVal "x10" (CPS.CLitFloat 10.0) $ CPS.CLetPrim "res" (CPS.CBinOp OpLT) ["i", "x10"] $ CPS.CLetCont "k1" "x1" (CPS.CLetPrim "res2" (CPS.CBinOp OpAdd) ["x", "i"] $ CPS.CAppFun "g" "l" "res2") $ CPS.CLetCont "k2" "x2" (CPS.CAppCont "l" "i") $ CPS.CIf "res" "k1" "k2") (CPS.CLetVal "x0" (CPS.CLitFloat 0.0) $ CPS.CAppFun "g" "k" "x0")
        
gSsa :: SFnDef
gSsa = SFnDef "f" [SArg "x"] (SBlock [SAssign "x0" $ SLitFloat 0.0, SGoto $ SLabel "g"]) 
    [ SLabelled (SLabel "g") [SPhiNode "i" [(SLabel "f_init", SArg "x0"), (SLabel "k1", SArg "res2")]] $ SBlock [SAssign "x10" $ SLitFloat 10.0, SAssign "res" $ SLT (SVar "i") (SVar "x10"), SIf (SVar "res") (SBlock [SGoto $ SLabel "k1"]) (SBlock [SGoto $ SLabel "k2"])]
    , SLabelled (SLabel "k2") [] $ SBlock [SReturn (SVar "i")]
    , SLabelled (SLabel "k1") [] $ SBlock [SAssign "res2" $ SAdd (SVar "x") (SVar "i"), SGoto $ SLabel "g"] ]
