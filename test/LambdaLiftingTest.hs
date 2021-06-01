module LambdaLiftingTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Identity (Identity(..))
import qualified Data.Set as Set

import Core.AST
import Core.LambdaLifting (lambdaLiftProgs)
import Core.Ops as Ops (BinOp(..))
import Core.Types

tests :: TestTree
tests = testGroup "Lambda lifting tests"
    [ testCase "let f a b = let g x = a + x in g b" $ lambdaLiftProgs [typedProg1] @?= liftedTypedProgs1
    , testCase "let f x = let g i = if i < 10 then g (x + i) else i in g 0" $ lambdaLiftProgs [typedProg2] @?= liftedTypedProgs2 ]
    where
        typedProg1 = Prog (VarId "f" $ Identity (TFun TFloat (TFun TFloat TFloat))) [VarId "x" $ Identity TFloat, VarId "y" $ Identity TFloat] $ 
            LetFun (Prog (VarId "g" $ Identity (TFun TFloat TFloat)) [VarId "z" $ Identity TFloat] $ BinOp OpAdd (Var . VarId "x" $ Identity TFloat) (Var . VarId "z" $ Identity TFloat)) (App (Var . VarId "g" . Identity $ TFun TFloat TFloat) (Var . VarId "y" $ Identity TFloat))
        liftedTypedProgs1 = Set.toList $ Set.fromList [ Prog (VarId "f" $ Identity (TFun TFloat (TFun TFloat TFloat))) [VarId "x" $ Identity TFloat, VarId "y" $ Identity TFloat] $ App (App (Var $ VarId "g" $ Identity (TFun TFloat (TFun TFloat TFloat))) (Var . VarId "x" $ Identity TFloat)) (Var . VarId "y" $ Identity TFloat)
                                                      , Prog (VarId "g" $ Identity (TFun TFloat (TFun TFloat TFloat))) [VarId "x" $ Identity TFloat, VarId "z" $ Identity TFloat] $ BinOp OpAdd (Var . VarId "x" $ Identity TFloat) (Var . VarId "z" $ Identity TFloat) ]
        typedProg2 = Prog (VarId "f" $ Identity (TFun TFloat TFloat)) [VarId "x" $ Identity TFloat] $ 
            LetFun (Prog (VarId "g" $ Identity (TFun TFloat TFloat)) [VarId "i" $ Identity TFloat] $ If (BinOp OpLT (Var $ VarId "i" $ Identity TFloat) (Lit $ LFloat 10.0)) (App (Var $ VarId "g" $ Identity $ TFun TFloat TFloat) (BinOp OpAdd (Var $ VarId "x" $ Identity TFloat) (Var $ VarId "i" $ Identity TFloat))) (Var $ VarId "i" $ Identity TFloat)) (App (Var . VarId "g" . Identity $ TFun TFloat TFloat) (Lit $ LFloat 0.0))
        liftedTypedProgs2 = Set.toList $ Set.fromList [ Prog (VarId "f" $ Identity (TFun TFloat TFloat)) [VarId "x" $ Identity TFloat] $ App (App (Var $ VarId "g" $ Identity (TFun TFloat (TFun TFloat TFloat))) (Var . VarId "x" $ Identity TFloat)) (Lit $ LFloat 0.0)
                                                      , Prog (VarId "g" $ Identity (TFun TFloat (TFun TFloat TFloat))) [VarId "x" $ Identity TFloat, VarId "i" $ Identity TFloat] $ If (BinOp OpLT (Var $ VarId "i" $ Identity TFloat) (Lit $ LFloat 10.0)) (App (App (Var $ VarId "g" $ Identity (TFun TFloat (TFun TFloat TFloat))) (Var $ VarId "x" $ Identity TFloat)) (BinOp OpAdd (Var $ VarId "x" $ Identity TFloat) (Var $ VarId "i" $ Identity TFloat))) (Var $ VarId "i" $ Identity TFloat) ]
