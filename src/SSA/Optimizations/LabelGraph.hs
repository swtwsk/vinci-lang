module SSA.Optimizations.LabelGraph where

import Data.Bifunctor (bimap)
import qualified Data.Map as Map

import SSA.AST

data Edge = NoEdge | JumpEdge SLabel | BranchEdge SLabel SLabel
type BlocksMap = Map.Map SLabel SLabelledBlock
type Edges   = Map.Map SLabel Edge

-- GRAPH CREATION
blocksToGraph :: [SLabelledBlock] -> (BlocksMap, Edges)
blocksToGraph (lb@(SLabelled l _ b):t) = 
    bimap (Map.insert l lb) (Map.insert l (blockToEdge b)) (blocksToGraph t)
blocksToGraph [] = (Map.empty, Map.empty)

blockToEdge :: SBlock -> Edge
blockToEdge (SBlock stmts) = case safeLast stmts of
    Just (SGoto l) -> JumpEdge l
    Just (SIf _ l1 l2) -> BranchEdge l1 l2
    _ -> NoEdge
    where
        safeLast [x] = Just x
        safeLast (_:t) = safeLast t
        safeLast [] = Nothing
