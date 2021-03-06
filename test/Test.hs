import Test.Tasty

import qualified AlphaEqTest
import qualified ParserTests
import qualified CoreToCPSTest
import qualified LambdaLiftingTest
import qualified TranspileToSSATest
import qualified TypeInferenceTest
import qualified InterpreterTests.CoreTest as CoreInterpreterTest
import qualified InterpreterTests.SSATest as SSAInterpreterTest
import qualified OptimizationTests.PostOrderTest as PostOrderTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
    [ AlphaEqTest.tests
    , interpretersTests
    , ParserTests.tests
    , CoreToCPSTest.tests
    , TranspileToSSATest.tests
    , LambdaLiftingTest.tests
    , PostOrderTest.tests
    , TypeInferenceTest.tests ]

interpretersTests :: TestTree
interpretersTests = testGroup "Interpreters" [ CoreInterpreterTest.tests
                                             , SSAInterpreterTest.tests ]
