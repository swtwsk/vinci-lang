import Test.Tasty

import qualified AlphaEqTest
import qualified ParserTests
import qualified CoreToCPSTest
import qualified TranspileToSSATest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
    [ AlphaEqTest.tests
    , ParserTests.tests
    , CoreToCPSTest.tests
    , TranspileToSSATest.tests ]
