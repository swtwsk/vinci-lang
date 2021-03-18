module ParserTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified AST.AbsVinci as Abs
import AST.ErrM (Err(..))
import AST.ParVinci (pLine, myLexer)

import Frontend.AST
import Frontend.TranspileAST (transpile)

tests :: TestTree
tests = testGroup "Parser tests" [ arithmeticTests, tupleTests, structTests ]

arithmeticTests = testGroup "Arithmetics"
    [ testCase "Addition" $ assertLine addLine addAbsLine
    , testCase "Transpiled addition" $ assertTranspiledLine addLine addASTLine
    , testCase "Multiplication and division" $ assertLine divLine divAbsLine
    , testCase "Transpiled multiplication and division" $ assertTranspiledLine divLine divASTLine
    , testCase "Comparison" $ assertLine lessThanLine lessThanAbsLine
    , testCase "Transpiled comparison" $ assertTranspiledLine lessThanLine lessThanASTLine ]
    where
        addLine = "1 + 1;;"
        addAbsLine = Abs.Line () (Abs.Expression () (Abs.EAdd () (Abs.EInt () 1) (Abs.EInt () 1)))
        addASTLine = Line (Expression (EAdd (EInt 1) (EInt 1)))
        divLine = "2 * 3.0 / 4;;"
        divAbsLine = Abs.Line () (Abs.Expression () (Abs.EDiv () (Abs.EMul () (Abs.EInt () 2) (Abs.EFloat () 3.0)) (Abs.EInt () 4)))
        divASTLine = Line (Expression (EDiv (EMul (EInt 2) (EFloat 3.0)) (EInt 4)))
        lessThanLine = "x < 15;;"
        lessThanAbsLine = Abs.Line () (Abs.Expression () (Abs.ELTH () (Abs.EId () (Abs.VIdent "x")) (Abs.EInt () 15)))
        lessThanASTLine = Line (Expression (ELTH (EId "x") (EInt 15)))

tupleTests = testGroup "Tuple"
    [ testCase "Tuple Expr" $ assertLine exprLine exprAbsLine 
    , testCase "Transpiled Tuple Expr" $ assertTranspiledLine exprLine exprASTLine 
    , testCase "Tuple Deconstruction" $ assertLine deconstructLine deconstructAbsLine 
    , testCase "Transpiled Tuple Deconstruction" $ assertTranspiledLine deconstructLine deconstructASTLine
    , testCase "Wildcarded deconstruction" $ assertLine wildcardLine wildcardAbsLine
    , testCase "Transpiled wildcarded deconstruction" $ assertTranspiledLine wildcardLine wildcardASTLine
    , testCase "Tuple Deconstruction Syntax Error" $ expectSyntaxError syntaxErrorLine ]
    where
        exprLine = "let a = (1.0, 0.5);;"
        exprAbsLine = Abs.Line () (Abs.Value () (Abs.Let () [Abs.ConstBind () (Abs.LetLVI () (Abs.LambdaVId () (Abs.VIdent "a"))) (Abs.ETuple () (Abs.EFloat () 1.0) [Abs.EFloat () 0.5])]))
        exprASTLine = Line (Value (Let [ConstBind (LambdaVId "a") (ETuple [EFloat 1.0, EFloat 0.5])]))
        deconstructLine = "let (a, b) = (\\x, y -> x, 42);;"
        deconstructAbsLine = Abs.Line () (Abs.Value () (Abs.Let () [Abs.ConstBind () (Abs.LetLVI () (Abs.TupleVId () [Abs.LambdaVId () (Abs.VIdent "a"), Abs.LambdaVId () (Abs.VIdent "b")])) (Abs.ETuple () (Abs.ELambda () [Abs.LambdaVId () (Abs.VIdent "x"), Abs.LambdaVId () (Abs.VIdent "y")] (Abs.EId () (Abs.VIdent "x"))) [Abs.EInt () 42])]))
        deconstructASTLine = Line (Value (Let [ConstBind (TupleVId [LambdaVId "a", LambdaVId "b"]) (ETuple [ELambda [LambdaVId "x", LambdaVId "y"] (EId "x"), EInt 42])]))
        syntaxErrorLine = "let a, b = \\x -> x, 42;;"
        wildcardLine = "let (_, y) = f (0.1, 0.2, 0.3);;"
        wildcardAbsLine = Abs.Line () (Abs.Value () (Abs.Let () [Abs.ConstBind () (Abs.LetLVI () (Abs.TupleVId () [Abs.WildVId (), Abs.LambdaVId () (Abs.VIdent "y")])) (Abs.EApp () (Abs.EId () (Abs.VIdent "f")) (Abs.ETuple () (Abs.EFloat () 0.1) [Abs.EFloat () 0.2, Abs.EFloat () 0.3]))]))
        wildcardASTLine = Line (Value (Let [ConstBind (TupleVId [WildVId, LambdaVId "y"]) (EApp (EId "f") (ETuple [EFloat 0.1, EFloat 0.2, EFloat 0.3]))]))

structTests = testGroup "Struct"
    [ testCase "Struct declaration" $ assertLine declareLine declareAbsLine
    , testCase "Transpiled Struct declaration" $ assertTranspiledLine declareLine declareASTLine 
    , testCase "Named constructor" $ assertLine constructLine constructAbsLine
    , testCase "Transpiled named constructor" $ assertTranspiledLine constructLine constructASTLine
    , testCase "Function returning an unnamed constructor" $ assertLine returnLine returnAbsLine
    , testCase "Transpiled function returning an unnamed constructor" $ assertTranspiledLine returnLine returnASTLine
    , testCase "Get field" $ assertLine fieldGetLine fieldGetAbsLine
    , testCase "Transpiled get field" $ assertTranspiledLine fieldGetLine fieldGetASTLine ]
    where
        declareLine = "struct MyStruct \'a {\n field1 : Int , field2 : Float, field3 : \'a, field4 : MyStruct2 };;"
        declareAbsLine = Abs.Line () (Abs.StructDecl () (Abs.SDef () (Abs.SIdent "MyStruct") [Abs.TPolyIdent "\'a"] [Abs.FieldDecl () (Abs.VIdent "field1") (Abs.TInt ()), Abs.FieldDecl () (Abs.VIdent "field2") (Abs.TFloat ()), Abs.FieldDecl () (Abs.VIdent "field3") (Abs.TPoly () (Abs.TPolyIdent "\'a")), Abs.FieldDecl () (Abs.VIdent "field4") (Abs.TStruct () (Abs.SIdent "MyStruct2"))]))
        declareASTLine = Line (StructDecl (SDef "MyStruct" ["\'a"] [FieldDecl "field1" TInt, FieldDecl "field2" TFloat, FieldDecl "field3" (TPoly "\'a"), FieldDecl "field4" (TStruct "MyStruct2")]))
        constructLine = "let res = MyStruct { field1 = True, outs=(a, b, 0.123)\n};;"
        constructAbsLine = Abs.Line () (Abs.Value () (Abs.Let () [Abs.ConstBind () (Abs.LetLVI () (Abs.LambdaVId () (Abs.VIdent "res"))) (Abs.ENamedCons () (Abs.SIdent "MyStruct") [Abs.FieldDef () (Abs.VIdent "field1") (Abs.ETrue ()), Abs.FieldDef () (Abs.VIdent "outs") (Abs.ETuple () (Abs.EId () (Abs.VIdent "a")) [Abs.EId () (Abs.VIdent "b"), Abs.EFloat () 0.123])])]))
        constructASTLine = Line (Value (Let [ConstBind (LambdaVId "res") (ENamedCons "MyStruct" [FieldDef "field1" ETrue, FieldDef "outs" (ETuple [EId "a", EId "b", EFloat 0.123])])]))
        returnLine = "let proc glPos -> VertOuts = { gl_Position = glPos };;"
        returnAbsLine = Abs.Line () (Abs.Value () (Abs.Let () [Abs.ProcBind () (Abs.ProcNameId () (Abs.VIdent "proc")) [Abs.LetLVI () (Abs.LambdaVId () (Abs.VIdent "glPos"))] (Abs.RetType () (Abs.TStruct () (Abs.SIdent "VertOuts"))) (Abs.ECons () [Abs.FieldDef () (Abs.VIdent "gl_Position") (Abs.EId () (Abs.VIdent "glPos"))])]))
        returnASTLine = Line (Value (Let [ProcBind "proc" [LambdaVId "glPos"] (Just (TStruct "VertOuts")) (ECons [FieldDef "gl_Position" (EId "glPos")])]))
        fieldGetLine = "outs.x;;"
        fieldGetAbsLine = Abs.Line () (Abs.Expression () (Abs.EFieldGet () (Abs.EId () (Abs.VIdent "outs")) (Abs.VIdent "x")))
        fieldGetASTLine = Line (Expression (EFieldGet (EId "outs") "x"))

lexLine :: String -> Err (Abs.Line ())
lexLine = pLine . myLexer

assertLine :: String -> Abs.Line () -> Assertion
assertLine line absLine = case lexLine line of
    Ok t -> t @?= absLine
    Bad f -> assertFailure f

assertTranspiledLine :: String -> Line -> Assertion
assertTranspiledLine line astLine = case lexLine line of
    Ok t -> transpile t @?= astLine
    Bad f -> assertFailure f

expectSyntaxError :: String -> Assertion
expectSyntaxError line = case lexLine line of
    Ok t -> assertFailure $ "expected syntax error but got " ++ show t
    Bad _ -> return ()
