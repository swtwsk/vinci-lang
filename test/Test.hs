import Test.Tasty

import qualified AlphaEqTest
import qualified ParserTests
import qualified CoreToCPSTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
    [ AlphaEqTest.tests
    , ParserTests.tests
    , CoreToCPSTest.tests ]
