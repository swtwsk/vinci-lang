import Test.Tasty

import qualified ParserTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ParserTests.tests]
