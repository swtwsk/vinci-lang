module SSA.Optimizations.GraphOptimizations where

import qualified Data.Map as Map

import SSA.AST (SBlock(..), SLabel(..), SLabelledBlock(..), SFnDef(..))
import SSA.Optimizations.SinglePhiElimination (eliminateSinglePhi)
import SSA.Optimizations.CleanControlFlow (cleanControlFlow, countPrecedessors)
import SSA.Optimizations.LabelGraph (BlocksMap, blocksToGraph)

optimizeGraph :: SFnDef -> SFnDef
optimizeGraph (SFnDef fName rType args block lbs) =
    SFnDef fName rType args block' (snd <$> Map.toList blockMap'')
    where
        (blockMap, edges) = blocksToGraph lbs
        (edges', blockMap') = (cleanControlFlow edges . eliminateSinglePhi) blockMap
        initPrec = countPrecedessors edges' (SLabel $ fName ++ "_init2")
        (block', blockMap'') = 
            if initPrec == 0 
            then mergeInits fName block blockMap' 
            else (block, blockMap')

mergeInits :: String -> SBlock -> BlocksMap -> (SBlock, BlocksMap)
mergeInits fName block blocksMap = 
    if null phis then (block2, Map.delete fInit2 blocksMap) else (block, blocksMap)
    where
        fInit2 = SLabel $ fName ++ "_init2"
        (SLabelled _ phis block2) = blocksMap Map.! fInit2
