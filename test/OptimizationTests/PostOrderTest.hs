module OptimizationTests.PostOrderTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Map as Map

import SSA.AST
import SSA.LabelGraph (Edge(..), postOrder)

tests :: TestTree
tests = testGroup "Post order tests"
    [ testCase "Empty" $ postOrder Map.empty @?= []
    , testCase "Two sons" $ postOrder twoSonsGraph @?= twoSonsPost
    , testCase "Full graph" $ postOrder fullGraph @?= fullPost ]
    where
        twoSonsGraph = Map.fromList [ (SLabel "a", BranchEdge (SLabel "b") (SLabel "c"))
                                    , (SLabel "b", NoEdge)
                                    , (SLabel "c", NoEdge)]
        twoSonsPost  = [SLabel "b", SLabel "c", SLabel "a"]
        fullGraph    = Map.fromList [ (SLabel "a", BranchEdge (SLabel "b") (SLabel "c"))
                                    , (SLabel "b", BranchEdge (SLabel "d") (SLabel "e"))
                                    , (SLabel "c", BranchEdge (SLabel "e") (SLabel "f"))
                                    , (SLabel "d", NoEdge)
                                    , (SLabel "e", BranchEdge (SLabel "d") (SLabel "c"))
                                    , (SLabel "f", NoEdge)]
        fullPost     = [ SLabel "d", SLabel "f", SLabel "c"
                       , SLabel "e", SLabel "b", SLabel "a" ]
