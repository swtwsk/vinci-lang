module LambdaLiftingTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Set as Set

import Core.AST
import Core.LambdaLifting (lambdaLiftProg)
import Core.Ops as Ops (BinOp(..))

tests :: TestTree
tests = testGroup "Lambda lifting tests"
    [ testCase "let f a b = let g x = a + x in g b" $ lambdaLiftProg prog1 @?= liftedProgs1
    , testCase "let f a b = let g x = a + x in g b, typed" $ lambdaLiftProg typedProg1 @?= liftedTypedProgs1
    , testCase "let f x = let g i = if i < 10 then g (x + i) else i in g 0, typed" $ lambdaLiftProg typedProg2 @?= liftedTypedProgs2 ]
    where
        prog1 = Prog (VarId "f" Nothing) [VarId "x" Nothing, VarId "y" Nothing] $ 
            LetFun (Prog (VarId "g" Nothing) [VarId "z" Nothing] $ BinOp OpAdd (Var $ VarId "x" Nothing) (Var $ VarId "z" Nothing)) (App (Var . VarId "g" $ Nothing) (Var $ VarId "y" Nothing))
        liftedProgs1 = Set.toList $ Set.fromList [ Prog (VarId "f" Nothing) [VarId "x" Nothing, VarId "y" Nothing] $ App (App (Var $ VarId "g" Nothing) (Var $ VarId "x" Nothing)) (Var $ VarId "y" Nothing)
                                                 , Prog (VarId "g" Nothing) [VarId "x" Nothing, VarId "z" Nothing] $ BinOp OpAdd (Var $ VarId "x" Nothing) (Var $ VarId "z" Nothing) ]
        typedProg1 = Prog (VarId "f" $ Just (TFun TFloat (TFun TFloat TFloat))) [VarId "x" $ Just TFloat, VarId "y" $ Just TFloat] $ 
            LetFun (Prog (VarId "g" $ Just (TFun TFloat TFloat)) [VarId "z" $ Just TFloat] $ BinOp OpAdd (Var . VarId "x" $ Just TFloat) (Var . VarId "z" $ Just TFloat)) (App (Var . VarId "g" . Just $ TFun TFloat TFloat) (Var . VarId "y" $ Just TFloat))
        liftedTypedProgs1 = Set.toList $ Set.fromList [ Prog (VarId "f" $ Just (TFun TFloat (TFun TFloat TFloat))) [VarId "x" $ Just TFloat, VarId "y" $ Just TFloat] $ App (App (Var $ VarId "g" $ Just (TFun TFloat (TFun TFloat TFloat))) (Var . VarId "x" $ Just TFloat)) (Var . VarId "y" $ Just TFloat)
                                                      , Prog (VarId "g" $ Just (TFun TFloat (TFun TFloat TFloat))) [VarId "x" $ Just TFloat, VarId "z" $ Just TFloat] $ BinOp OpAdd (Var . VarId "x" $ Just TFloat) (Var . VarId "z" $ Just TFloat) ]
        typedProg2 = Prog (VarId "f" $ Just (TFun TFloat TFloat)) [VarId "x" $ Just TFloat] $ 
            LetFun (Prog (VarId "g" $ Just (TFun TFloat TFloat)) [VarId "i" $ Just TFloat] $ If (BinOp OpLT (Var $ VarId "i" $ Just TFloat) (Lit $ LFloat 10.0)) (App (Var $ VarId "g" $ Just $ TFun TFloat TFloat) (BinOp OpAdd (Var $ VarId "x" $ Just TFloat) (Var $ VarId "i" $ Just TFloat))) (Var $ VarId "i" $ Just TFloat)) (App (Var . VarId "g" . Just $ TFun TFloat TFloat) (Lit $ LFloat 0.0))
        liftedTypedProgs2 = Set.toList $ Set.fromList [ Prog (VarId "f" $ Just (TFun TFloat TFloat)) [VarId "x" $ Just TFloat] $ App (App (Var $ VarId "g" $ Just (TFun TFloat (TFun TFloat TFloat))) (Var . VarId "x" $ Just TFloat)) (Lit $ LFloat 0.0)
                                                      , Prog (VarId "g" $ Just (TFun TFloat (TFun TFloat TFloat))) [VarId "x" $ Just TFloat, VarId "i" $ Just TFloat] $ If (BinOp OpLT (Var $ VarId "i" $ Just TFloat) (Lit $ LFloat 10.0)) (App (App (Var $ VarId "g" $ Just (TFun TFloat (TFun TFloat TFloat))) (Var $ VarId "x" $ Just TFloat)) (BinOp OpAdd (Var $ VarId "x" $ Just TFloat) (Var $ VarId "i" $ Just TFloat))) (Var $ VarId "i" $ Just TFloat) ]
