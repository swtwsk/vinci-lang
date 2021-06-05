module LibraryList ( intToFloat
                   , floatToInt
                   , coreLibraryList 
                   , librarySchemeList
                   , spirLibraryList
                   , spirStructureList ) where

import qualified Data.Map as Map

import Core.Ops
import Core.Types
import StructDefMap (StructDefMap)
import qualified SPIRV.Types as SType (SpirType(..))

type SpirTypeToFunName = SType.SpirType -> String

intToFloat :: String
intToFloat = "intToFloat"

floatToInt :: String
floatToInt = "floatToInt"

-- based on the list of GLSL.std.450 extensions from
-- https://www.khronos.org/registry/spir-v/specs/1.0/GLSL.std.450.html
libraryList :: Map.Map String (Scheme, SpirTypeToFunName)
libraryList = Map.fromList 
    [ ("round",       (floatingUnOpScheme, const "Round"))
    , ("roundEven",   (floatingUnOpScheme, const "RoundEven"))
    , ("trunc",       (floatingUnOpScheme, const "Trunc"))
    , ("abs",         (unNumOpScheme, floatOrIntFunName "FAbs" "SAbs"))
    , ("sign",        (unNumOpScheme, floatOrIntFunName "FSign" "SSign"))
    , ("floor",       (floatingUnOpScheme, const "Floor"))
    , ("ceil",        (floatingUnOpScheme, const "Ceil"))
    , ("fract",       (floatingUnOpScheme, const "Fract"))
    , ("radians",     (floatingUnOpScheme, const "Radians"))
    , ("degrees",     (floatingUnOpScheme, const "Degrees"))
    , ("sin",         (floatingUnOpScheme, const "Sin"))
    , ("cos",         (floatingUnOpScheme, const "Cos"))
    , ("tan",         (floatingUnOpScheme, const "Tan"))
    , ("asin",        (floatingUnOpScheme, const "Asin"))
    , ("acos",        (floatingUnOpScheme, const "Acos"))
    , ("atan",        (floatingUnOpScheme, const "Atan"))
    , ("sinh",        (floatingUnOpScheme, const "Sinh"))
    , ("cosh",        (floatingUnOpScheme, const "Cosh"))
    , ("tanh",        (floatingUnOpScheme, const "Tanh"))
    , ("asinh",       (floatingUnOpScheme, const "Asinh"))
    , ("acosh",       (floatingUnOpScheme, const "Acosh"))
    , ("atanh",       (floatingUnOpScheme, const "Atanh"))
    , ("atan2",       (floatingBinOpScheme, const "Atan2"))
    , ("pow",         (floatingBinOpScheme, const "Pow"))
    , ("exp",         (floatingUnOpScheme, const "Exp"))
    , ("log",         (floatingUnOpScheme, const "Log"))
    , ("exp2",        (floatingUnOpScheme, const "Exp2"))
    , ("log2",        (floatingUnOpScheme, const "Log2"))
    , ("sqrt",        (floatingUnOpScheme, const "Sqrt"))
    , ("inversesqrt", (floatingUnOpScheme, const "InverseSqrt"))
    , ("min",         (binNumOpScheme, floatOrIntFunName "FMin" "SMin"))
    , ("max",         (binNumOpScheme, floatOrIntFunName "FMax" "SMax"))
    , ("clamp",       (tripleNumOpScheme, floatOrIntFunName "FClamp" "SClamp"))
    , ("mix",         (floatingTripleOpScheme, const "FMin"))
    , ("step",        (floatingBinOpScheme, const "Step"))
    , ("smoothstep",  (floatingTripleOpScheme, const "SmoothStep"))
    , ("length",      (floatingUnOpToScalarScheme, const "Length"))
    , ("distance",    (floatingBinOpToScalarScheme, const "Distance"))
    , ("cross",       (Scheme [] ([] :=> TFun (TTuple TFloat 3) (TFun (TTuple TFloat 3) (TTuple TFloat 3))), const "Cross"))
    , ("normalize",   (floatingUnOpScheme, const "Normalize"))
    , ("faceforward", (floatingTripleOpScheme, const "FaceForward"))
    , ("reflect",     (floatingBinOpScheme, const "Reflect"))
    , ("refract",     (Scheme [Tyvar "a"] ([IsIn ClassFloating (TVar $ Tyvar "a")] :=> TFun (TVar $ Tyvar "a") (TFun (TVar $ Tyvar "a") (TFun TFloat (TVar $ Tyvar "a")))), const "Refract"))
    , (floatToInt,    (Scheme [] ([] :=> TFun TFloat TInt), undefined))
    , (intToFloat,    (Scheme [] ([] :=> TFun TInt TFloat), undefined))
    ]
    where
        floatOrIntFunName floatName intName t = case t of
            SType.TFloat       -> floatName
            SType.TInt         -> intName
            SType.TVector t' _ -> floatOrIntFunName floatName intName t'
            _ -> undefined -- it's typechecked at this point anyway

        unNumOpScheme  = unOpScheme ClassNum
        binNumOpScheme = binOpScheme ClassNum
        tripleNumOpScheme = tripleOpScheme ClassNum

        floatingUnOpScheme = unOpScheme ClassFloating
        floatingBinOpScheme = binOpScheme ClassFloating
        floatingTripleOpScheme = tripleOpScheme ClassFloating
        floatingUnOpToScalarScheme = Scheme [Tyvar "a"] ([IsIn ClassFloating (TVar $ Tyvar "a")] :=> 
            TFun (TVar $ Tyvar "a") TFloat)
        floatingBinOpToScalarScheme = Scheme [Tyvar "a"] ([IsIn ClassFloating (TVar $ Tyvar "a")] :=> 
            TFun (TVar $ Tyvar "a") (TFun (TVar $ Tyvar "a") TFloat))

librarySchemeList :: Map.Map String Scheme
librarySchemeList = Map.map fst libraryList `Map.union` Map.fromList
    [ (show OpAdd,   binNumOpScheme)
    , (show OpMul,   binNumOpScheme)
    , (show OpSub,   binNumOpScheme)
    , (show OpDiv,   binNumOpScheme)
    , (show OpMod,   binNumOpScheme)
    , (show OpAnd,   boolBinOpScheme)
    , (show OpOr,    boolBinOpScheme)
    , (show OpEq,    binBoolEqOpScheme)
    , (show OpNotEq, binBoolEqOpScheme)
    , (show OpLT,    binBoolOrdOpScheme)
    , (show OpLTEq,  binBoolOrdOpScheme)
    , (show OpGT,    binBoolOrdOpScheme)
    , (show OpGTEq,  binBoolOrdOpScheme)
    , (show OpNeg,   unNumScheme)
    , (show OpNot,   constTypedUnOpScheme TBool) ]
    where
        binOpBoolScheme :: Class -> Scheme
        binOpBoolScheme c = Scheme [Tyvar "a"] ([IsIn c (TVar $ Tyvar "a")] :=> 
            TFun (TVar $ Tyvar "a") (TFun (TVar $ Tyvar "a") TBool))
        
        constTypedBinOpScheme :: Type -> Scheme
        constTypedBinOpScheme t = Scheme [] ([] :=> TFun t (TFun t t))
        constTypedUnOpScheme :: Type -> Scheme
        constTypedUnOpScheme t = Scheme [] ([] :=> TFun t t)

        binBoolEqOpScheme  = binOpBoolScheme ClassEq
        binBoolOrdOpScheme = binOpBoolScheme ClassOrd
        binNumOpScheme = binOpScheme ClassNum
        unNumScheme    = unOpScheme ClassNum
        boolBinOpScheme  = constTypedBinOpScheme TBool

coreLibraryList :: Map.Map String Type
coreLibraryList = Map.map (fromScheme . fst) libraryList

spirLibraryList :: Map.Map String SpirTypeToFunName
spirLibraryList = Map.map snd libraryList

spirStructureList :: StructDefMap SType.SpirType
spirStructureList = Map.fromList 
    [ ("gl_PerVertex", 
        [ ("gl_Position", Nothing, SType.TVector SType.TFloat 4)
        , ("gl_PointSize", Nothing, SType.TFloat)
        , ("gl_ClipDistance", Nothing, SType.TArray SType.TFloat 1) 
        , ("gl_CullDistance", Nothing, SType.TArray SType.TFloat 1) 
        ])
    ]

-- Helpers
tripleOpScheme :: Class -> Scheme
tripleOpScheme c = Scheme [Tyvar "a"] $ 
    [IsIn c (TVar $ Tyvar "a")] :=> 
        TFun (TVar $ Tyvar "a") (TFun (TVar $ Tyvar "a") (TFun (TVar $ Tyvar "a") (TVar $ Tyvar "a")))

binOpScheme :: Class -> Scheme
binOpScheme c = Scheme [Tyvar "a"] $ 
    [IsIn c (TVar $ Tyvar "a")] :=> 
        TFun (TVar $ Tyvar "a") (TFun (TVar $ Tyvar "a") (TVar $ Tyvar "a"))

unOpScheme :: Class -> Scheme
unOpScheme c = Scheme [Tyvar "a"] ([IsIn c (TVar $ Tyvar "a")] :=> 
    TFun (TVar $ Tyvar "a") (TVar $ Tyvar "a"))
