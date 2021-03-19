module ParserTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified AST.AbsVinci as Abs
import AST.ErrM (Err(..))
import AST.ParVinci (pLine, myLexer, pProgram)

import Frontend.AST
import Frontend.TranspileAST (transpile)

tests :: TestTree
tests = testGroup "Parser tests" [ arithmeticTests, conditionalTests, functionTests, tupleTests, structTests, programTests ]

arithmeticTests = testGroup "Arithmetics"
    [ testCase "Addition" $ assertLine addLine addAbsLine
    , testCase "Transpiled addition" $ assertTranspiledLine addLine addASTLine
    , testCase "Subtraction" $ assertLine subtractLine subtractAbsLine
    , testCase "Transpiled subtraction" $ assertTranspiledLine subtractLine subtractASTLine
    , testCase "Modulo" $ assertLine modLine modAbsLine
    , testCase "Transpiled modulo" $ assertTranspiledLine modLine modASTLine
    , testCase "Multiplication and division" $ assertLine divLine divAbsLine
    , testCase "Transpiled multiplication and division" $ assertTranspiledLine divLine divASTLine
    , testCase "Comparison Less Than" $ assertLine lessThanLine lessThanAbsLine
    , testCase "Transpiled comparison Less Than" $ assertTranspiledLine lessThanLine lessThanASTLine
    , testCase "Comparison Greater Than" $ assertLine greaterThanLine greaterThanAbsLine
    , testCase "Transpiled comparison Greater Than" $ assertTranspiledLine greaterThanLine greaterThanASTLine ]
    where
        addLine = "1 + 1;;"
        addAbsLine = Abs.Line () (Abs.Expression () (Abs.EAdd () (Abs.EInt () 1) (Abs.EInt () 1)))
        addASTLine = Line (Expression (EAdd (EInt 1) (EInt 1)))
        subtractLine = "2 - 2;;"
        subtractAbsLine = Abs.Line () (Abs.Expression () (Abs.ESub () (Abs.EInt () 2) (Abs.EInt () 2)))
        subtractASTLine = Line (Expression (ESub (EInt 2) (EInt 2)))
        divLine = "2 * 3.0 / 4;;"
        divAbsLine = Abs.Line () (Abs.Expression () (Abs.EDiv () (Abs.EMul () (Abs.EInt () 2) (Abs.EFloat () 3.0)) (Abs.EInt () 4)))
        divASTLine = Line (Expression (EDiv (EMul (EInt 2) (EFloat 3.0)) (EInt 4)))
        modLine = "10 % 5 == 0;;"
        modAbsLine = Abs.Line () (Abs.Expression () (Abs.EEQU () (Abs.EMod () (Abs.EInt () 10) (Abs.EInt () 5)) (Abs.EInt () 0)))
        modASTLine = Line (Expression (EEQU (EMod (EInt 10) (EInt 5)) (EInt 0)))
        lessThanLine = "x < 15;;"
        lessThanAbsLine = Abs.Line () (Abs.Expression () (Abs.ELTH () (Abs.EId () (Abs.VIdent "x")) (Abs.EInt () 15)))
        lessThanASTLine = Line (Expression (ELTH (EId "x") (EInt 15)))
        greaterThanLine = "x > -5;;"
        greaterThanAbsLine = Abs.Line () (Abs.Expression () (Abs.EGTH () (Abs.EId () (Abs.VIdent "x")) (Abs.ENeg () (Abs.EInt () 5))))
        greaterThanASTLine = Line (Expression (EGTH (EId "x") (ENeg (EInt 5))))

conditionalTests = testGroup "Conditional"
    [ testCase "if-then-else" $ assertLine ifLine ifAbsLine
    , testCase "Transpiled if-then-else" $ assertTranspiledLine ifLine ifASTLine
    , testCase "if-then syntax error" $ expectSyntaxError ifSyntaxErrorLine
    , testCase "_ or _ and _" $ assertLine andOrLine andOrAbsLine
    , testCase "Transpiled _ or _ and _" $ assertTranspiledLine andOrLine andOrASTLine ]
    where
        ifLine = "if not False then 42 else 112;;"
        ifAbsLine = Abs.Line () (Abs.Expression () (Abs.ECond () (Abs.ENot () (Abs.EFalse ())) (Abs.EInt () 42) (Abs.EInt () 112)))
        ifASTLine = Line (Expression (ECond (ENot EFalse) (EInt 42) (EInt 112)))
        ifSyntaxErrorLine = "if True then 42;;"
        andOrLine = "x <= 3 or x >= 5 and x != 6;;"
        andOrAbsLine = Abs.Line () (Abs.Expression () (Abs.EOr () (Abs.ELE () (Abs.EId () (Abs.VIdent "x")) (Abs.EInt () 3)) (Abs.EAnd () (Abs.EGE () (Abs.EId () (Abs.VIdent "x")) (Abs.EInt () 5)) (Abs.ENE () (Abs.EId () (Abs.VIdent "x")) (Abs.EInt () 6)))))
        andOrASTLine = Line (Expression (EOr (ELE (EId "x") (EInt 3)) (EAnd (EGE (EId "x") (EInt 5)) (ENE (EId "x") (EInt 6)))))

functionTests = testGroup "Function"
    [ testCase "letrec" $ assertLine recLine recAbsLine
    , testCase "Traspiled letrec" $ assertTranspiledLine recLine recASTLine
    , testCase "Typed lambda" $ assertLine typedLambdaLine typedLambdaAbsLine
    , testCase "Transpiled typed lambda" $ assertTranspiledLine typedLambdaLine typedLambdaASTLine ]
    where
        recLine = "letrec l i c =\n  if i == limit then c\n  else l (i + 1) (c + 1);;"
        recAbsLine = Abs.Line () (Abs.Value () (Abs.LetRec () [Abs.ProcBind () (Abs.ProcNameId () (Abs.VIdent "l")) [Abs.LetLVI () (Abs.LambdaVId () (Abs.VIdent "i")), Abs.LetLVI () (Abs.LambdaVId () (Abs.VIdent "c"))] (Abs.NoRetType ()) (Abs.ECond () (Abs.EEQU () (Abs.EId () (Abs.VIdent "i")) (Abs.EId () (Abs.VIdent "limit"))) (Abs.EId () (Abs.VIdent "c")) (Abs.EApp () (Abs.EApp () (Abs.EId () (Abs.VIdent "l")) (Abs.EAdd () (Abs.EId () (Abs.VIdent "i")) (Abs.EInt () 1))) (Abs.EAdd () (Abs.EId () (Abs.VIdent "c")) (Abs.EInt () 1))))]))
        recASTLine = Line (Value (LetRec [ProcBind "l" [LambdaVId "i", LambdaVId "c"] Nothing (ECond (EEQU (EId "i") (EId "limit")) (EId "c") (EApp (EApp (EId "l") (EAdd (EId "i") (EInt 1))) (EAdd (EId "c") (EInt 1))))]))
        typedLambdaLine = "(\\x, y -> x : \'a -> \'b -> \'a);;"
        typedLambdaAbsLine = Abs.Line () (Abs.Expression () (Abs.ETyped () (Abs.ELambda () [Abs.LambdaVId () (Abs.VIdent "x"), Abs.LambdaVId () (Abs.VIdent "y")] (Abs.EId () (Abs.VIdent "x"))) (Abs.TFun () (Abs.TPoly () (Abs.TPolyIdent "\'a")) (Abs.TFun () (Abs.TPoly () (Abs.TPolyIdent "\'b")) (Abs.TPoly () (Abs.TPolyIdent "\'a"))))))
        typedLambdaASTLine = Line (Expression (ETyped (ELambda [LambdaVId "x", LambdaVId "y"] (EId "x")) (TFun (TPoly "\'a") (TFun (TPoly "\'b") (TPoly "\'a")))))

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

programTests = testGroup "Program" 
    [ testCase "Program" $ assertProgram program programAbs
    , testCase "Transpiled program" $ assertTranspiledProgram program programAST ]
    where
        program = "let x = 0.42;;\n\nstruct Maybe \'a {\n  value: \'a,\n  flag : Bool\n};;\n\nlet frag (fragColor, fragTexCoord) (_ : Uniforms) =\n  let color = (fragColor.x, fragColor.y, x)\n  in (color, 1.0);;"
        programAbs = Abs.Prog () [Abs.Value () (Abs.Let () [Abs.ConstBind () (Abs.LetLVI () (Abs.LambdaVId () (Abs.VIdent "x"))) (Abs.EFloat () 0.42)]), Abs.StructDecl () (Abs.SDef () (Abs.SIdent "Maybe") [Abs.TPolyIdent "\'a"] [Abs.FieldDecl () (Abs.VIdent "value") (Abs.TPoly () (Abs.TPolyIdent "\'a")), Abs.FieldDecl () (Abs.VIdent "flag") (Abs.TBool ())]), Abs.Value () (Abs.Let () [Abs.ProcBind () (Abs.ProcNameId () (Abs.VIdent "frag")) [Abs.LetLVI () (Abs.TupleVId () [Abs.LambdaVId () (Abs.VIdent "fragColor"), Abs.LambdaVId () (Abs.VIdent "fragTexCoord")]), Abs.LetLVI () (Abs.TypedVId () (Abs.WildVId ()) (Abs.TStruct () (Abs.SIdent "Uniforms")))] (Abs.NoRetType ()) (Abs.ELetIn () (Abs.Let () [Abs.ConstBind () (Abs.LetLVI () (Abs.LambdaVId () (Abs.VIdent "color"))) (Abs.ETuple () (Abs.EFieldGet () (Abs.EId () (Abs.VIdent "fragColor")) (Abs.VIdent "x")) [Abs.EFieldGet () (Abs.EId () (Abs.VIdent "fragColor")) (Abs.VIdent "y"), Abs.EId () (Abs.VIdent "x")])]) (Abs.ETuple () (Abs.EId () (Abs.VIdent "color")) [Abs.EFloat () 1.0]))])]
        programAST = Prog [Value $ Let [ConstBind (LambdaVId "x") (EFloat 0.42)], StructDecl $ SDef "Maybe" ["\'a"] [FieldDecl "value" (TPoly "\'a"), FieldDecl "flag" TBool], Value $ Let [ProcBind "frag" [TupleVId [LambdaVId "fragColor", LambdaVId "fragTexCoord"], TypedVId WildVId (TStruct "Uniforms")] Nothing (ELetIn (Let [ConstBind (LambdaVId "color") (ETuple [EFieldGet (EId "fragColor") "x", EFieldGet (EId "fragColor") "y", EId "x"])]) (ETuple [EId "color", EFloat 1.0]))]]

lexProgram :: String -> Err (Abs.Program ())
lexProgram = pProgram . myLexer

assertProgram :: String -> Abs.Program () -> Assertion
assertProgram program absProgram = case lexProgram program of
    Ok t -> t @?= absProgram
    Bad f -> assertFailure f

assertTranspiledProgram :: String -> Program -> Assertion
assertTranspiledProgram program astProgram = case lexProgram program of
    Ok t -> transpile t @?= astProgram
    Bad f -> assertFailure f

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
