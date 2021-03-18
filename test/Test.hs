import Test.Tasty
import Test.Tasty.HUnit

import qualified ParserTests

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ParserTests.tests]
