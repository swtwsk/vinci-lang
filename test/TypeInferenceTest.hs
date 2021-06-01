module TypeInferenceTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as Map

import Core.AST
import Core.CoreManager
import Core.Ops
import Core.TypeChecking
import Core.Types

tests :: TestTree
tests = testGroup "Type inference tests" [ typeInferenceTest 
                                         , structInferenceTest ]

typeInferenceTest :: TestTree
typeInferenceTest = testGroup "Type inference"
    [ testCase "id" $ tiCoreManagerToType (cm idProg) @?= Right [tIdTy]
    , testCase "idProg" $ eitherManagerToProg (tiCoreManager (cm idProg)) @?= [tIdProg]
    , testCase "floatId" $ tiCoreManagerToType (cm floatIdProg) @?= Right [tFloatIdTy]
    , testCase "idProg with type hint" $ eitherManagerToProg (tiCoreManager (cm floatIdProg)) @?= [tFloatIdProg]
    , testCase "const" $ tiCoreManagerToType (cm constProg) @?= Right [tConstTy]
    , testCase "constProg" $ eitherManagerToProg (tiCoreManager (cm constProg)) @?= [tConstProg]
    , testCase "const with type hint" $ tiCoreManagerToType (cm const2Prog) @?= Right [tConst2Ty]
    , testCase "constProg with type hint" $ eitherManagerToProg (tiCoreManager (cm const2Prog)) @?= [tConst2Prog]
    , testCase "addId" $ tiCoreManagerToType (cm letIdProg) @?= Right [tLetIdTy]
    , testCase "addId2" $ tiCoreManagerToType (cm letId2Prog) @?= Right [tLetId2Ty]
    , testCase "add" $ tiCoreManagerToType (cm addProg) @?= Right [tAddTy]
    , testCase "addProg" $ eitherManagerToProg (tiCoreManager $ cm addProg) @?= [tAddProg]
    , testCase "wrongAdd" $ tiCoreManagerToType (cm wrongAddProg) @?= Left "Bool is not a member of class Num"
    , testCase "eq" $ tiCoreManagerToType (cm goodEqProg) @?= Right [tEqTy]
    , testCase "let y = x in y" $ eitherManagerToProg (tiCoreManager $ cm letProg) @?= [tLetProg]
    ]
    where
        idProg = Prog (VarId "id" Nothing) [VarId "x" Nothing] (Var $ VarId "x" Nothing)
        tIdTy = TFun (TVar $ Tyvar "a2") (TVar $ Tyvar "a2")
        tIdProg = Prog (Var' "id" tIdTy) [Var' "x" (TVar $ Tyvar "a2")] (Var $ Var' "x" (TVar $ Tyvar "a2"))
        floatIdProg = Prog (VarId "id" Nothing) [VarId "x" (Just TFloat)] (Var $ VarId "x" Nothing)
        tFloatIdTy = TFun TFloat TFloat
        tFloatIdProg = Prog (Var' "id" tFloatIdTy) [Var' "x" TFloat] (Var $ Var' "x" TFloat)
        constProg = Prog (VarId "const" Nothing) [VarId "x" Nothing, VarId "y" Nothing] (Var $ VarId "x" Nothing)
        tConstTy = TFun (TVar $ Tyvar "a3") $ TFun (TVar $ Tyvar "a4") (TVar $ Tyvar "a3")
        tConstProg = Prog (Var' "const" tConstTy) [Var' "x" (TVar $ Tyvar "a3"), Var' "y" (TVar $ Tyvar "a4")] (Var $ Var' "x" (TVar $ Tyvar "a3"))
        const2Prog = Prog (VarId "const" Nothing) [VarId "x" (Just $ TVar $ Tyvar "\'x"), VarId "y" (Just $ TVar $ Tyvar "\'x")] (Var $ VarId "x" Nothing)
        tConst2Ty = TFun (TVar $ Tyvar "a1") $ TFun (TVar $ Tyvar "a1") (TVar $ Tyvar "a1")
        tConst2Prog = Prog (Var' "const" tConst2Ty) [Var' "x" (TVar $ Tyvar "a1"), Var' "y" (TVar $ Tyvar "a1")] (Var $ Var' "x" (TVar $ Tyvar "a1"))
        addProg = Prog (VarId "add" Nothing) [VarId "x" Nothing, VarId "y" Nothing] (BinOp OpAdd (Var $ VarId "x" Nothing) (Var $ VarId "y" Nothing))
        tAddTy = TFun (TVar $ Tyvar "a6") (TFun (TVar $ Tyvar "a6") (TVar $ Tyvar "a6"))
        tAddProg = Prog (Var' "add" tAddTy) [Var' "x" (TVar $ Tyvar "a6"), Var' "y" (TVar $ Tyvar "a6")] (BinOp OpAdd (Var $ Var' "x" (TVar $ Tyvar "a6")) (Var $ Var' "y" (TVar $ Tyvar "a6")))
        wrongAddProg = Prog (VarId "wrong" Nothing) [VarId "x" Nothing] (BinOp OpAdd (Var $ VarId "x" Nothing) (Lit $ LBool True))
        goodEqProg = Prog (VarId "eq" Nothing) [VarId "x" Nothing] (BinOp OpEq (Var $ VarId "x" Nothing) (Lit $ LBool True))
        tEqTy = TFun TBool TBool
        letIdProg = Prog (VarId "f" Nothing) [VarId "x" Nothing] $ LetFun idProg (BinOp OpAdd (App (Var $ VarId "id" Nothing) (Lit $ LInt 2)) (App (Var $ VarId "id" Nothing) (Var $ VarId "x" Nothing)))
        tLetIdTy = TFun TInt TInt
        letId2Prog = Prog (VarId "f" Nothing) [VarId "x" Nothing] $
            LetFun idProg (BinOp OpAdd (App (Var $ VarId "id" Nothing) (Var $ VarId "x" Nothing)) (App (Var $ VarId "id" Nothing) (Var $ VarId "x" Nothing)))
        tLetId2Ty = TFun (TVar $ Tyvar "a12") (TVar $ Tyvar "a12")
        letProg = Prog (VarId "f" Nothing) [VarId "x" Nothing] $ Let (VarId "y" Nothing) (Var $ VarId "x" Nothing) (Var $ VarId "y" Nothing)
        tLetProg = Prog (Var' "f" $ TFun (TVar $ Tyvar "a2") (TVar $ Tyvar "a2")) [Var' "x" $ TVar $ Tyvar "a2"] $ Let (Var' "y" $ TVar $ Tyvar "a2") (Var . Var' "x" $ TVar $ Tyvar "a2") (Var . Var' "y" $ TVar $ Tyvar "a2")

structInferenceTest :: TestTree
structInferenceTest = testGroup "Struct inference"
    [ testCase "cons" $ _progs <$> tiCoreManager (cm' consProg) @?= Right [tConsProg]
    , testCase "cons and get" $ _progs <$> tiCoreManager (cm' consGetProg) @?= Right [tConsGetProg]
    , testCase "cons and get, poly" $ eitherManagerToProg (tiCoreManager (cm2' consGetProg)) @?= [tConsGetProgPoly]
    , testCase "cons and get inner struct" $ _progs <$> tiCoreManager (cm' consGet2Prog) @?= Right [tConsGet2Prog] ]
    where
        cm' p = CoreManager [p] (Map.fromList [ ("MyStruct", [("myField", Nothing, TFloat)] ) 
                                              , ("MyStruct2", [("a", Nothing, TInt), ("b", Nothing, TStruct "MyStruct")])
                                              ])
        cm2' p = CoreManager [p] (Map.singleton "MyStruct" [("myField", Nothing, TVar $ Tyvar "a")])

        consProg = Prog (VarId "f" Nothing) [VarId "x" Nothing] $ Cons "MyStruct" [Var $ VarId "x" Nothing]
        tConsProg = Prog (Var' "f" $ TFun TFloat (TStruct "MyStruct")) [Var' "x" TFloat] $ Cons "MyStruct" [Var $ Var' "x" TFloat]
        consGetProg = Prog (VarId "f" Nothing) [VarId "x" Nothing] $ Let (VarId "s" Nothing) (Cons "MyStruct" [Var $ VarId "x" Nothing]) (FieldGet "myField" (Var $ VarId "s" Nothing))
        tConsGetProg = Prog (Var' "f" $ TFun TFloat TFloat) [Var' "x" TFloat] $ Let (Var' "s" $ TStruct "MyStruct") (Cons "MyStruct" [Var $ Var' "x" TFloat]) (FieldGet "myField" (Var $ Var' "s" $ TStruct "MyStruct"))
        tConsGetProgPoly = Prog (Var' "f" $ TFun (TVar $ Tyvar "a2") (TVar $ Tyvar "a2")) [Var' "x" $ TVar $ Tyvar "a2"] $ Let (Var' "s" $ TStruct "MyStruct") (Cons "MyStruct" [Var $ Var' "x" (TVar $ Tyvar "a2")]) (FieldGet "myField" (Var $ Var' "s" $ TStruct "MyStruct"))
        consGet2Prog = Prog (VarId "f" Nothing) [VarId "x" Nothing, VarId "y" Nothing] $ Let (VarId "s" Nothing) (Cons "MyStruct2" [Var $ VarId "x" Nothing, Var $ VarId "y" Nothing]) (FieldGet "myField" (FieldGet "b" (Var $ VarId "s" Nothing)))
        tConsGet2Prog = Prog (Var' "f" $ TFun TInt (TFun (TStruct "MyStruct") TFloat)) [Var' "x" TInt, Var' "y" (TStruct "MyStruct")] $ Let (Var' "s" $ TStruct "MyStruct2") (Cons "MyStruct2" [Var $ Var' "x" TInt, Var $ Var' "y" (TStruct "MyStruct")]) (FieldGet "myField" (FieldGet "b" (Var $ Var' "s" $ TStruct "MyStruct2")))

cm :: Prog f -> CoreManager f
cm p = CoreManager [p] Map.empty

eitherManagerToProg :: Either a (CoreManager f) -> [Prog f]
eitherManagerToProg (Left _) = []
eitherManagerToProg (Right coreManager) = _progs coreManager
