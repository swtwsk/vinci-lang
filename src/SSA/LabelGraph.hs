module SSA.LabelGraph (
    Edge(..),
    BlocksMap,
    Edges,
    blocksToGraph,
    postOrder
) where

import Control.Monad.RWS
import Data.Bifunctor (bimap, first, second)
import Data.DList (DList, toList)
import qualified Data.Map as Map
import qualified Data.Set as Set

import SSA.AST
import Utils.DList (output)

data Edge      = NoEdge | JumpEdge SLabel | BranchEdge SLabel SLabel 
               deriving Show
type BlocksMap = Map.Map SLabel SLabelledBlock
type Edges     = Map.Map SLabel Edge

type Visited = Set.Set SLabel
type DFSM    = RWS Edges (DList SLabel) (Visited, Int)

-- GRAPH CREATION
blocksToGraph :: [SLabelledBlock] -> (BlocksMap, Edges)
blocksToGraph (lb@(SLabelled l _ b):t) = 
    bimap (Map.insert l lb) (Map.insert l (blockToEdge b)) (blocksToGraph t)
blocksToGraph [] = (Map.empty, Map.empty)

blockToEdge :: SBlock -> Edge
blockToEdge (SBlock stmts) = case safeLast stmts of
    Just (SGoto l) -> JumpEdge l
    Just (SIf _ _ l1 l2) -> BranchEdge l1 l2
    _ -> NoEdge
    where
        safeLast [x] = Just x
        safeLast (_:t) = safeLast t
        safeLast [] = Nothing

-- Post Order
postOrder :: Edges -> [SLabel]
postOrder edges = toList postOrdered
    where
        (_, postOrdered) = execRWS (mapM_ postOrder' vertexList) edges initState
        vertexList = fst <$> Map.toList edges
        initState = (Set.empty, 0)

postOrder' :: SLabel -> DFSM ()
postOrder' l = do
    visited <- gets fst
    if Set.member l visited then return () else do
        modify $ first (Set.insert l)
        edge <- asks (Map.! l)
        case edge of
            NoEdge -> return ()
            JumpEdge l' -> postOrder' l'
            BranchEdge l1 l2 -> postOrder' l1 >> postOrder' l2
        i <- gets snd
        output l 
        modify . second . const $ i + 1
