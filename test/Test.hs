import Test.Tasty
import Test.Tasty.HUnit

import AST.AbsVinci
import AST.ErrM ( Err(..) )
import AST.ParVinci ( pLine, myLexer )

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      lexLine "let a = (1.0, 0.5);;" @?= Ok (Line () (Value () (Let () [ConstBind () (LetLVI () (LambdaVId () (VIdent "a"))) (ETuple () (EFloat () 1.0) [EFloat () 0.5])])))
  ]
  where
      lexLine = pLine . myLexer
