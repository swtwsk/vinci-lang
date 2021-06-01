module LibraryList ( coreLibraryList 
                   , librarySchemeList
                   , spirLibraryList
                   , spirStructureList ) where

import qualified Data.Map as Map

import Core.Ops
import Core.Types
import StructDefMap (StructDefMap)
import qualified SPIRV.Types as SType (SpirType(..))

libraryList :: Map.Map String (Scheme, String)
libraryList = Map.fromList 
    [ ("sin",   (floatUnOpScheme, "Sin"))
    , ("cos",   (floatUnOpScheme, "Cos"))
    , ("sqrt",  (floatUnOpScheme, "Sqrt"))
    , ("abs",   (floatUnOpScheme, "FAbs"))
    , ("floor", (floatUnOpScheme, "Floor"))
    , ("ceil",  (floatUnOpScheme, "Ceil"))
    , ("fract", (floatUnOpScheme, "Fract"))
    , ("clamp", (Scheme [] ([] :=> TFun TFloat (TFun TFloat (TFun TFloat TFloat))), "FClamp")) ]
    where       
        floatUnOpScheme = Scheme [] ([] :=> TFun TFloat TFloat)

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
        binOpScheme :: Class -> Scheme
        binOpScheme c = Scheme [Tyvar "a"] ([IsIn c (TVar $ Tyvar "a")] :=> 
            TFun (TVar $ Tyvar "a") (TFun (TVar $ Tyvar "a") (TVar $ Tyvar "a")))
        binOpBoolScheme :: Class -> Scheme
        binOpBoolScheme c = Scheme [Tyvar "a"] ([IsIn c (TVar $ Tyvar "a")] :=> 
            TFun (TVar $ Tyvar "a") (TFun (TVar $ Tyvar "a") TBool))
        unOpScheme :: Class -> Scheme
        unOpScheme c = Scheme [Tyvar "a"] ([IsIn c (TVar $ Tyvar "a")] :=> 
            TFun (TVar $ Tyvar "a") (TVar $ Tyvar "a"))
        
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

spirLibraryList :: Map.Map String String
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
