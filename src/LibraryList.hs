module LibraryList ( LibraryFunction (..)
                   , readLibraryFunction
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

data LibraryFunction = IntToFloat
                     | FloatToInt
                     | Texture1D
                     | Texture2D
                     | Texture3D
                     | LibFun String
                     deriving (Eq, Ord, Read)

readLibraryFunction :: String -> LibraryFunction
readLibraryFunction libFun = case libFun of
    "intToFloat" -> IntToFloat
    "floatToInt" -> FloatToInt
    "texture1D"  -> Texture1D
    "texture2D"  -> Texture2D
    "texture3D"  -> Texture3D
    s -> LibFun s

-- based on the list of GLSL.std.450 extensions from
-- https://www.khronos.org/registry/spir-v/specs/1.0/GLSL.std.450.html
libraryList :: Map.Map LibraryFunction (Scheme, SpirTypeToFunName)
libraryList = Map.fromList 
    [ (LibFun "round",       (floatingUnOpScheme, const "Round"))
    , (LibFun "roundEven",   (floatingUnOpScheme, const "RoundEven"))
    , (LibFun "trunc",       (floatingUnOpScheme, const "Trunc"))
    , (LibFun "abs",         (unNumOpScheme, floatOrIntFunName "FAbs" "SAbs"))
    , (LibFun "sign",        (unNumOpScheme, floatOrIntFunName "FSign" "SSign"))
    , (LibFun "floor",       (floatingUnOpScheme, const "Floor"))
    , (LibFun "ceil",        (floatingUnOpScheme, const "Ceil"))
    , (LibFun "fract",       (floatingUnOpScheme, const "Fract"))
    , (LibFun "radians",     (floatingUnOpScheme, const "Radians"))
    , (LibFun "degrees",     (floatingUnOpScheme, const "Degrees"))
    , (LibFun "sin",         (floatingUnOpScheme, const "Sin"))
    , (LibFun "cos",         (floatingUnOpScheme, const "Cos"))
    , (LibFun "tan",         (floatingUnOpScheme, const "Tan"))
    , (LibFun "asin",        (floatingUnOpScheme, const "Asin"))
    , (LibFun "acos",        (floatingUnOpScheme, const "Acos"))
    , (LibFun "atan",        (floatingUnOpScheme, const "Atan"))
    , (LibFun "sinh",        (floatingUnOpScheme, const "Sinh"))
    , (LibFun "cosh",        (floatingUnOpScheme, const "Cosh"))
    , (LibFun "tanh",        (floatingUnOpScheme, const "Tanh"))
    , (LibFun "asinh",       (floatingUnOpScheme, const "Asinh"))
    , (LibFun "acosh",       (floatingUnOpScheme, const "Acosh"))
    , (LibFun "atanh",       (floatingUnOpScheme, const "Atanh"))
    , (LibFun "atan2",       (floatingBinOpScheme, const "Atan2"))
    , (LibFun "pow",         (floatingBinOpScheme, const "Pow"))
    , (LibFun "exp",         (floatingUnOpScheme, const "Exp"))
    , (LibFun "log",         (floatingUnOpScheme, const "Log"))
    , (LibFun "exp2",        (floatingUnOpScheme, const "Exp2"))
    , (LibFun "log2",        (floatingUnOpScheme, const "Log2"))
    , (LibFun "sqrt",        (floatingUnOpScheme, const "Sqrt"))
    , (LibFun "inversesqrt", (floatingUnOpScheme, const "InverseSqrt"))
    , (LibFun "min",         (binNumOpScheme, floatOrIntFunName "FMin" "SMin"))
    , (LibFun "max",         (binNumOpScheme, floatOrIntFunName "FMax" "SMax"))
    , (LibFun "clamp",       (tripleNumOpScheme, floatOrIntFunName "FClamp" "SClamp"))
    , (LibFun "mix",         (floatingTripleOpScheme, const "FMin"))
    , (LibFun "step",        (floatingBinOpScheme, const "Step"))
    , (LibFun "smoothstep",  (floatingTripleOpScheme, const "SmoothStep"))
    , (LibFun "length",      (floatingUnOpToScalarScheme, const "Length"))
    , (LibFun "distance",    (floatingBinOpToScalarScheme, const "Distance"))
    , (LibFun "cross",       (Scheme [] ([] :=> TFun (TTuple TFloat 3) (TFun (TTuple TFloat 3) (TTuple TFloat 3))), const "Cross"))
    , (LibFun "normalize",   (floatingUnOpScheme, const "Normalize"))
    , (LibFun "faceforward", (floatingTripleOpScheme, const "FaceForward"))
    , (LibFun "reflect",     (floatingBinOpScheme, const "Reflect"))
    , (LibFun "refract",     (Scheme [Tyvar "a"] ([IsIn ClassFloating (TVar $ Tyvar "a")] :=> TFun (TVar $ Tyvar "a") (TFun (TVar $ Tyvar "a") (TFun TFloat (TVar $ Tyvar "a")))), const "Refract"))
    , (Texture1D,     (Scheme [] ([] :=> TFun (TSampler 1) (TFun TFloat (TTuple TFloat 4))), undefined))
    , (Texture2D,     (Scheme [] ([] :=> TFun (TSampler 2) (TFun (TTuple TFloat 2) (TTuple TFloat 4))), undefined))
    , (Texture3D,     (Scheme [] ([] :=> TFun (TSampler 3) (TFun (TTuple TFloat 3) (TTuple TFloat 4))), undefined))
    , (FloatToInt,    (Scheme [] ([] :=> TFun TFloat TInt), undefined))
    , (IntToFloat,    (Scheme [] ([] :=> TFun TInt TFloat), undefined))
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
librarySchemeList = bimapMap show fst libraryList `Map.union` Map.fromList
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
coreLibraryList = bimapMap show (fromScheme . fst) libraryList

spirLibraryList :: Map.Map LibraryFunction SpirTypeToFunName
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
bimapMap :: (Ord k1, Ord k2) => 
            (k1 -> k2) 
         -> (a -> b) 
         -> Map.Map k1 a 
         -> Map.Map k2 b
bimapMap keyFn valueFn = Map.mapKeys keyFn . Map.map valueFn

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

instance Show LibraryFunction where
    show Texture1D  = "texture1D"
    show Texture2D  = "texture2D"
    show Texture3D  = "texture3D"
    show IntToFloat = "intToFloat"
    show FloatToInt = "floatToInt"
    show (LibFun f) = f
