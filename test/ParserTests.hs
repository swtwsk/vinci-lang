module ParserTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Parser.AbsVinci as Abs
import Parser.ParVinci (pLine, myLexer, pProgram)

import Frontend.AST
import Frontend.TranspileAST (transpile)

type Err = Either String
type MaybePos = Maybe (Int, Int)

tests :: TestTree
tests = testGroup "Parser tests" [ arithmeticTests, conditionalTests, functionTests, tupleTests, structTests, programTests ]

arithmeticTests :: TestTree
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
        addAbsLine = Abs.Line Nothing (Abs.Expression Nothing (Abs.EAdd Nothing (Abs.EInt Nothing 1) (Abs.EInt Nothing 1)))
        addASTLine = Line (Expression (EAdd (EInt 1) (EInt 1)))
        subtractLine = "2 - 2;;"
        subtractAbsLine = Abs.Line Nothing (Abs.Expression Nothing (Abs.ESub Nothing (Abs.EInt Nothing 2) (Abs.EInt Nothing 2)))
        subtractASTLine = Line (Expression (ESub (EInt 2) (EInt 2)))
        divLine = "2 * 3.0 / 4;;"
        divAbsLine = Abs.Line Nothing (Abs.Expression Nothing (Abs.EDiv Nothing (Abs.EMul Nothing (Abs.EInt Nothing 2) (Abs.EFloat Nothing 3.0)) (Abs.EInt Nothing 4)))
        divASTLine = Line (Expression (EDiv (EMul (EInt 2) (EFloat 3.0)) (EInt 4)))
        modLine = "10 % 5 == 0;;"
        modAbsLine = Abs.Line Nothing (Abs.Expression Nothing (Abs.EEQU Nothing (Abs.EMod Nothing (Abs.EInt Nothing 10) (Abs.EInt Nothing 5)) (Abs.EInt Nothing 0)))
        modASTLine = Line (Expression (EEQU (EMod (EInt 10) (EInt 5)) (EInt 0)))
        lessThanLine = "x < 15;;"
        lessThanAbsLine = Abs.Line Nothing (Abs.Expression Nothing (Abs.ELTH Nothing (Abs.EId Nothing (Abs.VIdent "x")) (Abs.EInt Nothing 15)))
        lessThanASTLine = Line (Expression (ELTH (EId "x") (EInt 15)))
        greaterThanLine = "x > -5;;"
        greaterThanAbsLine = Abs.Line Nothing (Abs.Expression Nothing (Abs.EGTH Nothing (Abs.EId Nothing (Abs.VIdent "x")) (Abs.ENeg Nothing (Abs.EInt Nothing 5))))
        greaterThanASTLine = Line (Expression (EGTH (EId "x") (ENeg (EInt 5))))

conditionalTests :: TestTree
conditionalTests = testGroup "Conditional"
    [ testCase "if-then-else" $ assertLine ifLine ifAbsLine
    , testCase "Transpiled if-then-else" $ assertTranspiledLine ifLine ifASTLine
    , testCase "if-then syntax error" $ expectSyntaxError ifSyntaxErrorLine
    , testCase "_ or _ and _" $ assertLine andOrLine andOrAbsLine
    , testCase "Transpiled _ or _ and _" $ assertTranspiledLine andOrLine andOrASTLine ]
    where
        ifLine = "if not False then 42 else 112;;"
        ifAbsLine = Abs.Line Nothing (Abs.Expression Nothing (Abs.ECond Nothing (Abs.ENot Nothing (Abs.EFalse Nothing)) (Abs.EInt Nothing 42) (Abs.EInt Nothing 112)))
        ifASTLine = Line (Expression (ECond (ENot EFalse) (EInt 42) (EInt 112)))
        ifSyntaxErrorLine = "if True then 42;;"
        andOrLine = "x <= 3 or x >= 5 and x != 6;;"
        andOrAbsLine = Abs.Line Nothing (Abs.Expression Nothing (Abs.EOr Nothing (Abs.ELE Nothing (Abs.EId Nothing (Abs.VIdent "x")) (Abs.EInt Nothing 3)) (Abs.EAnd Nothing (Abs.EGE Nothing (Abs.EId Nothing (Abs.VIdent "x")) (Abs.EInt Nothing 5)) (Abs.ENE Nothing (Abs.EId Nothing (Abs.VIdent "x")) (Abs.EInt Nothing 6)))))
        andOrASTLine = Line (Expression (EOr (ELE (EId "x") (EInt 3)) (EAnd (EGE (EId "x") (EInt 5)) (ENE (EId "x") (EInt 6)))))

functionTests :: TestTree
functionTests = testGroup "Function"
    [ testCase "letrec" $ assertLine recLine recAbsLine
    , testCase "Traspiled letrec" $ assertTranspiledLine recLine recASTLine
    , testCase "Typed lambda" $ assertLine typedLambdaLine typedLambdaAbsLine
    , testCase "Transpiled typed lambda" $ assertTranspiledLine typedLambdaLine typedLambdaASTLine ]
    where
        recLine = "letrec l i c =\n  if i == limit then c\n  else l (i + 1) (c + 1);;"
        recAbsLine = Abs.Line Nothing (Abs.Value Nothing (Abs.LetRec Nothing [Abs.ProcBind Nothing (Abs.ProcNameId Nothing (Abs.VIdent "l")) [Abs.LetLVI Nothing (Abs.LambdaVId Nothing (Abs.VIdent "i")), Abs.LetLVI Nothing (Abs.LambdaVId Nothing (Abs.VIdent "c"))] (Abs.NoRetType Nothing) (Abs.ECond Nothing (Abs.EEQU Nothing (Abs.EId Nothing (Abs.VIdent "i")) (Abs.EId Nothing (Abs.VIdent "limit"))) (Abs.EId Nothing (Abs.VIdent "c")) (Abs.EApp Nothing (Abs.EApp Nothing (Abs.EId Nothing (Abs.VIdent "l")) (Abs.EAdd Nothing (Abs.EId Nothing (Abs.VIdent "i")) (Abs.EInt Nothing 1))) (Abs.EAdd Nothing (Abs.EId Nothing (Abs.VIdent "c")) (Abs.EInt Nothing 1))))]))
        recASTLine = Line (Value (LetRec [ProcBind "l" [LambdaVId "i", LambdaVId "c"] Nothing (ECond (EEQU (EId "i") (EId "limit")) (EId "c") (EApp (EApp (EId "l") (EAdd (EId "i") (EInt 1))) (EAdd (EId "c") (EInt 1))))]))
        typedLambdaLine = "(\\x, y -> x : \'a -> \'b -> \'a);;"
        typedLambdaAbsLine = Abs.Line Nothing (Abs.Expression Nothing (Abs.ETyped Nothing (Abs.ELambda Nothing [Abs.LambdaVId Nothing (Abs.VIdent "x"), Abs.LambdaVId Nothing (Abs.VIdent "y")] (Abs.EId Nothing (Abs.VIdent "x"))) (Abs.TFun Nothing (Abs.TPoly Nothing (Abs.TPolyIdent "\'a")) (Abs.TFun Nothing (Abs.TPoly Nothing (Abs.TPolyIdent "\'b")) (Abs.TPoly Nothing (Abs.TPolyIdent "\'a"))))))
        typedLambdaASTLine = Line (Expression (ETyped (ELambda [LambdaVId "x", LambdaVId "y"] (EId "x")) (TFun (TPoly "\'a") (TFun (TPoly "\'b") (TPoly "\'a")))))

tupleTests :: TestTree
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
        exprAbsLine = Abs.Line Nothing (Abs.Value Nothing (Abs.Let Nothing [Abs.ConstBind Nothing (Abs.LetLVI Nothing (Abs.LambdaVId Nothing (Abs.VIdent "a"))) (Abs.ETuple Nothing (Abs.EFloat Nothing 1.0) [Abs.EFloat Nothing 0.5])]))
        exprASTLine = Line (Value (Let [ConstBind (LambdaVId "a") (ETuple [EFloat 1.0, EFloat 0.5])]))
        deconstructLine = "let (a, b) = (\\x, y -> x, 42);;"
        deconstructAbsLine = Abs.Line Nothing (Abs.Value Nothing (Abs.Let Nothing [Abs.ConstBind Nothing (Abs.LetLVI Nothing (Abs.TupleVId Nothing [Abs.LambdaVId Nothing (Abs.VIdent "a"), Abs.LambdaVId Nothing (Abs.VIdent "b")])) (Abs.ETuple Nothing (Abs.ELambda Nothing [Abs.LambdaVId Nothing (Abs.VIdent "x"), Abs.LambdaVId Nothing (Abs.VIdent "y")] (Abs.EId Nothing (Abs.VIdent "x"))) [Abs.EInt Nothing 42])]))
        deconstructASTLine = Line (Value (Let [ConstBind (TupleVId [LambdaVId "a", LambdaVId "b"]) (ETuple [ELambda [LambdaVId "x", LambdaVId "y"] (EId "x"), EInt 42])]))
        syntaxErrorLine = "let a, b = \\x -> x, 42;;"
        wildcardLine = "let (_, y) = f (0.1, 0.2, 0.3);;"
        wildcardAbsLine = Abs.Line Nothing (Abs.Value Nothing (Abs.Let Nothing [Abs.ConstBind Nothing (Abs.LetLVI Nothing (Abs.TupleVId Nothing [Abs.WildVId Nothing, Abs.LambdaVId Nothing (Abs.VIdent "y")])) (Abs.EApp Nothing (Abs.EId Nothing (Abs.VIdent "f")) (Abs.ETuple Nothing (Abs.EFloat Nothing 0.1) [Abs.EFloat Nothing 0.2, Abs.EFloat Nothing 0.3]))]))
        wildcardASTLine = Line (Value (Let [ConstBind (TupleVId [WildVId, LambdaVId "y"]) (EApp (EId "f") (ETuple [EFloat 0.1, EFloat 0.2, EFloat 0.3]))]))

structTests :: TestTree
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
        declareAbsLine = Abs.Line Nothing (Abs.StructDecl Nothing (Abs.SDef Nothing (Abs.SIdent "MyStruct") [Abs.TPolyIdent "\'a"] [Abs.FieldDecl Nothing (Abs.VIdent "field1") (Abs.TInt Nothing), Abs.FieldDecl Nothing (Abs.VIdent "field2") (Abs.TFloat Nothing), Abs.FieldDecl Nothing (Abs.VIdent "field3") (Abs.TPoly Nothing (Abs.TPolyIdent "\'a")), Abs.FieldDecl Nothing (Abs.VIdent "field4") (Abs.TStruct Nothing (Abs.SIdent "MyStruct2"))]))
        declareASTLine = Line (StructDecl (SDef "MyStruct" ["\'a"] [FieldDecl "field1" TInt, FieldDecl "field2" TFloat, FieldDecl "field3" (TPoly "\'a"), FieldDecl "field4" (TStruct "MyStruct2")]))
        constructLine = "let res = MyStruct { field1 = True, outs=(a, b, 0.123)\n};;"
        constructAbsLine = Abs.Line Nothing (Abs.Value Nothing (Abs.Let Nothing [Abs.ConstBind Nothing (Abs.LetLVI Nothing (Abs.LambdaVId Nothing (Abs.VIdent "res"))) (Abs.ENamedCons Nothing (Abs.SIdent "MyStruct") [Abs.FieldDef Nothing (Abs.VIdent "field1") (Abs.ETrue Nothing), Abs.FieldDef Nothing (Abs.VIdent "outs") (Abs.ETuple Nothing (Abs.EId Nothing (Abs.VIdent "a")) [Abs.EId Nothing (Abs.VIdent "b"), Abs.EFloat Nothing 0.123])])]))
        constructASTLine = Line (Value (Let [ConstBind (LambdaVId "res") (ENamedCons "MyStruct" [FieldDef "field1" ETrue, FieldDef "outs" (ETuple [EId "a", EId "b", EFloat 0.123])])]))
        returnLine = "let proc glPos -> VertOuts = { gl_Position = glPos };;"
        returnAbsLine = Abs.Line Nothing (Abs.Value Nothing (Abs.Let Nothing [Abs.ProcBind Nothing (Abs.ProcNameId Nothing (Abs.VIdent "proc")) [Abs.LetLVI Nothing (Abs.LambdaVId Nothing (Abs.VIdent "glPos"))] (Abs.RetType Nothing (Abs.TStruct Nothing (Abs.SIdent "VertOuts"))) (Abs.ECons Nothing [Abs.FieldDef Nothing (Abs.VIdent "gl_Position") (Abs.EId Nothing (Abs.VIdent "glPos"))])]))
        returnASTLine = Line (Value (Let [ProcBind "proc" [LambdaVId "glPos"] (Just (TStruct "VertOuts")) (ECons [FieldDef "gl_Position" (EId "glPos")])]))
        fieldGetLine = "outs.x;;"
        fieldGetAbsLine = Abs.Line Nothing (Abs.Expression Nothing (Abs.EFieldGet Nothing (Abs.EId Nothing (Abs.VIdent "outs")) (Abs.VIdent "x")))
        fieldGetASTLine = Line (Expression (EFieldGet (EId "outs") "x"))

programTests :: TestTree
programTests = testGroup "Program" 
    [ testCase "Program" $ assertProgram program programAbs
    , testCase "Transpiled program" $ assertTranspiledProgram program programAST ]
    where
        program = "let x = 0.42;;\n\nstruct Maybe \'a {\n  value: \'a,\n  flag : Bool\n};;\n\nlet frag (fragColor, fragTexCoord) (_ : Uniforms) =\n  let color = (fragColor.x, fragColor.y, x)\n  in (color, 1.0);;"
        programAbs = Abs.Prog Nothing [Abs.Value Nothing (Abs.Let Nothing [Abs.ConstBind Nothing (Abs.LetLVI Nothing (Abs.LambdaVId Nothing (Abs.VIdent "x"))) (Abs.EFloat Nothing 0.42)]), Abs.StructDecl Nothing (Abs.SDef Nothing (Abs.SIdent "Maybe") [Abs.TPolyIdent "\'a"] [Abs.FieldDecl Nothing (Abs.VIdent "value") (Abs.TPoly Nothing (Abs.TPolyIdent "\'a")), Abs.FieldDecl Nothing (Abs.VIdent "flag") (Abs.TBool Nothing)]), Abs.Value Nothing (Abs.Let Nothing [Abs.ProcBind Nothing (Abs.ProcNameId Nothing (Abs.VIdent "frag")) [Abs.LetLVI Nothing (Abs.TupleVId Nothing [Abs.LambdaVId Nothing (Abs.VIdent "fragColor"), Abs.LambdaVId Nothing (Abs.VIdent "fragTexCoord")]), Abs.LetLVI Nothing (Abs.TypedVId Nothing (Abs.WildVId Nothing) (Abs.TStruct Nothing (Abs.SIdent "Uniforms")))] (Abs.NoRetType Nothing) (Abs.ELetIn Nothing (Abs.Let Nothing [Abs.ConstBind Nothing (Abs.LetLVI Nothing (Abs.LambdaVId Nothing (Abs.VIdent "color"))) (Abs.ETuple Nothing (Abs.EFieldGet Nothing (Abs.EId Nothing (Abs.VIdent "fragColor")) (Abs.VIdent "x")) [Abs.EFieldGet Nothing (Abs.EId Nothing (Abs.VIdent "fragColor")) (Abs.VIdent "y"), Abs.EId Nothing (Abs.VIdent "x")])]) (Abs.ETuple Nothing (Abs.EId Nothing (Abs.VIdent "color")) [Abs.EFloat Nothing 1.0]))])]
        programAST = Prog [Value $ Let [ConstBind (LambdaVId "x") (EFloat 0.42)], StructDecl $ SDef "Maybe" ["\'a"] [FieldDecl "value" (TPoly "\'a"), FieldDecl "flag" TBool], Value $ Let [ProcBind "frag" [TupleVId [LambdaVId "fragColor", LambdaVId "fragTexCoord"], TypedVId WildVId (TStruct "Uniforms")] Nothing (ELetIn (Let [ConstBind (LambdaVId "color") (ETuple [EFieldGet (EId "fragColor") "x", EFieldGet (EId "fragColor") "y", EId "x"])]) (ETuple [EId "color", EFloat 1.0]))]]

lexProgram :: String -> Err (Abs.Program MaybePos)
lexProgram = pProgram . myLexer

assertProgram :: String -> Abs.Program MaybePos -> Assertion
assertProgram program absProgram = case lexProgram program of
    Right t -> t @?= absProgram
    Left f -> assertFailure f

assertTranspiledProgram :: String -> Program -> Assertion
assertTranspiledProgram program astProgram = case lexProgram program of
    Right t -> transpile t @?= astProgram
    Left f -> assertFailure f

lexLine :: String -> Err (Abs.Line MaybePos)
lexLine = pLine . myLexer

assertLine :: String -> Abs.Line MaybePos -> Assertion
assertLine line absLine = case lexLine line of
    Right t -> t @?= absLine
    Left f -> assertFailure f

assertTranspiledLine :: String -> Line -> Assertion
assertTranspiledLine line astLine = case lexLine line of
    Right t -> transpile t @?= astLine
    Left f -> assertFailure f

expectSyntaxError :: String -> Assertion
expectSyntaxError line = case lexLine line of
    Right t -> assertFailure $ "expected syntax error but got " ++ show t
    Left _ -> return ()
