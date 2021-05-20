{-# LANGUAGE LambdaCase #-}
module SPIRV.DecorateOffsets (decorateOffsets) where

import Control.Monad.RWS
import Data.Bifunctor (second)
import Data.DList (DList, toList)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import qualified Data.Map as Map

import StructDefMap (StructDefMap)
import SPIRV.SpirOps
import SPIRV.Types
import Utils.DList (output)
import Utils.Tuple (trdTriple)

type ReaderEnv = Map.Map SpirType SpirId
type WList     = DList SpirOp
type StateEnv  = StructMap
type DecorateM = RWS ReaderEnv WList StateEnv

data StructMapField = Fields [SpirType] | SizeOf Int Int  -- size, alignment
type StructMap      = Map.Map SpirType StructMapField

decorateOffsets :: StructDefMap SpirType
                -> Map.Map SpirType SpirId
                -> [SpirOp]
decorateOffsets structs structIds = toList . snd $ 
    run (mapM_ tryDecorateOffset structsToDecorateList)
    where
        run f = evalRWS f structIds (toStructMap structs structsToDecorateList) 
        structsToDecorateList = flip filter (fst <$> Map.toList structIds) $
            \case { TStruct _ isUniform -> isUniform == Uniform ; _ -> False }

toStructMap :: StructDefMap SpirType 
            -> [SpirType]
            -> StructMap
toStructMap structDefs typesToDecorate = Map.union basicTypes
    (Map.fromList $ second Fields <$> fieldTypesList)
    where
        typeNames = catMaybes $ typesToDecorate <&> \case
            TStruct sName _ -> Just sName
            _ -> Nothing
        fieldTypesList = typeNames <&>
            \t -> (TStruct t Uniform, trdTriple <$> structDefs Map.! t)
        basicTypes = Map.fromList [ (TInt, SizeOf 4 4)
                                  , (TUnsignedInt, SizeOf 4 4)
                                  , (TFloat, SizeOf 4 4)
                                  , (TBool, SizeOf 4 4)
                                  , (TVector TFloat 2, SizeOf 8 8)
                                  , (TVector TFloat 3, SizeOf 12 16)
                                  , (TVector TFloat 4, SizeOf 16 16) ]

tryDecorateOffset :: SpirType -> DecorateM (Int, Int)
tryDecorateOffset spirType = do
    structOffset <- gets (Map.! spirType)
    case structOffset of
        Fields fields -> decorateOffsets' spirType fields
        SizeOf size alignment -> return (size, alignment)

decorateOffsets' :: SpirType -> [SpirType] -> DecorateM (Int, Int)
decorateOffsets' spirType fieldsTypes  = do
    typeId <- asks (Map.! spirType)
    -- fieldAlg <- extendedFieldAlignment fieldsTypes
    size <- aggregateOffsets 0 0 typeId fieldsTypes
    let alignment = 16  -- alignment for now, later matrix might break it?
    modify $ Map.insert spirType (SizeOf size alignment)
    return (size, alignment)

aggregateOffsets :: Int -> Int -> SpirId -> [SpirType] -> DecorateM Int
aggregateOffsets offset index spirId = \case
    ty:t -> do
        (size, align) <- tryDecorateOffset ty
        let aligned    = offset + padding align
            nextOffset = aligned + size
        output $ OpMemberDecorate spirId index Offset [Left aligned]
        aggregateOffsets nextOffset (index + 1) spirId t
    []   -> return $ offset + padding 16
    where
        padding align = (align - (offset `mod` align)) `mod` align

-- extendedFieldAlignment :: [SpirType] -> DecorateM Int
-- extendedFieldAlignment = foldM foldFn 0
--     where
--         foldFn acc el = max acc . snd <$> tryDecorateOffset el
