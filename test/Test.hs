import Test.Tasty

import qualified AlphaEqTest
import qualified ParserTests
import qualified CoreToCPSTest
import qualified TranspileToSSATest
import qualified InterpreterTests.CoreTest as CoreInterpreterTest
import qualified InterpreterTests.SSATest as SSAInterpreterTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
    [ AlphaEqTest.tests
    , interpretersTests
    , ParserTests.tests
    , CoreToCPSTest.tests
    , TranspileToSSATest.tests ]

interpretersTests :: TestTree
interpretersTests = testGroup "Interpreters" [ CoreInterpreterTest.tests
                                             , SSAInterpreterTest.tests ]
