{-# LANGUAGE TupleSections #-}
module SSA.OptimizeAndPrepare (optimizeAndPrepare) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative (Alternative((<|>)))
import Data.Bifunctor (first, second)
import Data.Functor ((<&>))
import Data.Graph (graphFromEdges, topSort)
import Data.Map (insert, toList, (!))
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set

import SSA.AST
import SSA.LabelGraph
import SSA.Optimizations.GraphOptimizations (optimizeGraph)

optimizeAndPrepare :: [SFnDef] -> [SFnDef]
optimizeAndPrepare = map (sortAndAnnotateBlocks . optimizeGraph)

sortAndAnnotateBlocks :: SFnDef -> SFnDef
sortAndAnnotateBlocks (SFnDef fName rType args block labelled) =
    SFnDef fName rType args block' sorted
    where
        initLabel = SLabel $ fName ++ "_init"
        firstBlockLabelled = SLabelled initLabel [] block
        (bMap, edges) = blocksToGraph (firstBlockLabelled:labelled)
        structuredLabelled = foldl (findAndAttachSCF edges) bMap $ 
            (\(SLabelled l _ _) -> l) <$> (firstBlockLabelled:labelled)
        labelled' = snd <$> toList structuredLabelled
        (block', labelled'') = extractFirstBlock labelled' initLabel
        sorted = blockTopoSort block' labelled''

extractFirstBlock :: [SLabelledBlock] -> SLabel -> (SBlock, [SLabelledBlock])
extractFirstBlock (lb@(SLabelled l _ block):t) initLabel =
    if l == initLabel then (block, t) else
        let (b, lbs) = extractFirstBlock t initLabel in (b, lb:lbs)
extractFirstBlock [] _ = undefined

-- CFG structured merge search
findAndAttachSCF :: Edges -> BlocksMap -> SLabel -> BlocksMap
findAndAttachSCF edges blocksMap l = case findStructuredControlFlow l edges of
    Just scf ->
        let (SLabelled l' phis (SBlock stmts)) = blocksMap ! l
            block' = SBlock $ attachStructuredControl stmts scf in
        insert l (SLabelled l' phis block') blocksMap
    Nothing -> blocksMap
    where
        attachStructuredControl :: [SStmt] -> SStructuredMerge -> [SStmt]
        attachStructuredControl [SIf _ cond l1 l2] scf = 
            [SIf (Just scf) cond l1 l2]
        attachStructuredControl (h:t) scf = h:attachStructuredControl t scf
        attachStructuredControl [] _ = []

findStructuredControlFlow :: SLabel -> Edges -> Maybe SStructuredMerge
findStructuredControlFlow l edges = case edges ! l of
    BranchEdge l1 l2 -> case findLoop l (l1, l2) edges of
        Just (loopLabel, breakLabel) -> Just (SLoopMerge breakLabel loopLabel)
        Nothing -> SSelectionMerge <$> findLca l1 l2 edges
    _ -> Nothing

type DFSM = ReaderT Edges (State (Set.Set SLabel))
findLoop :: SLabel -> (SLabel, SLabel) -> Edges -> Maybe (SLabel, SLabel)
findLoop l (l1, l2) edges = ((, l2) <$> runLoop l1) <|> ((, l1) <$> runLoop l2)
    where
        runLoop = \l' -> evalState (runReaderT (findLoop' l') edges) (Set.singleton l)

findLoop' :: SLabel -> DFSM (Maybe SLabel)
findLoop' l = do
    modify (Set.insert l)
    edge <- asks (! l)
    visited <- get
    let findLoopFn l' = if Set.member l' visited then return (Just l) else findLoop' l'
    case edge of
        NoEdge -> return Nothing
        JumpEdge jl -> findLoopFn jl
        BranchEdge l1 l2 -> (<|>) <$> findLoopFn l1 <*> findLoopFn l2

findLca :: SLabel -> SLabel -> Edges -> Maybe SLabel
findLca l1 l2 edges = listToMaybe (Set.toList lcas)
    where
        getL1Successors l = (l:) $ case edges ! l of
            NoEdge -> []
            JumpEdge jl -> getL1Successors jl
            BranchEdge jl1 jl2 -> getL1Successors jl1 ++ getL1Successors jl2
        (_:l1Successors) = getL1Successors l1  -- remove l1
        l1SuccSet = Set.fromList l1Successors
        getL2Successors l = Set.union (if Set.member l l1SuccSet then Set.singleton l else Set.empty) $ case edges ! l of
            NoEdge -> Set.empty
            JumpEdge jl -> getL2Successors jl
            BranchEdge jl1 jl2 -> getL2Successors jl1 `Set.union` getL2Successors jl2
        succSet = getL2Successors l2
        l2Successors = Set.toList $ Set.delete l2 succSet
        succSetFold = flip foldr succSet $ \el accSet -> case edges ! el of
            NoEdge -> accSet
            JumpEdge jl -> Set.delete jl accSet
            BranchEdge jl1 jl2 -> Set.delete jl1 (Set.delete jl2 accSet)
        lcas = succSetFold l2Successors

-- Sort topologically
type RemoveLoopM = State (Set.Set SLabel, Edges)

blockTopoSort :: SBlock -> [SLabelledBlock] -> [SLabelledBlock]
blockTopoSort (SBlock startStmts) labelled = sorted
    where
        (blocksMap, edges) = blocksToGraph labelled
        startLabels = getStartLabels startStmts
        startLabel = SLabel "start_label"
        (_, edges') = execState (removeLoops startLabel) 
                (Set.empty, insert startLabel startLabels edges)
        emptyNode = (SLabelled startLabel [] (SBlock []), startLabel, [])
        nodes = toList edges' <&> \(l, edge) -> (blocksMap ! l, l, ) $ 
            case edge of
                NoEdge -> []
                JumpEdge jl -> [jl]
                BranchEdge l1 l2 -> [l1, l2]
        (graph, nodeFromVertex, _) = graphFromEdges (emptyNode:nodes)
        fstTriple (x, _, _) = x
        emptyNodeFilter = filter (\(_, l, _) -> l /= startLabel)
        sorted = fstTriple <$> emptyNodeFilter (nodeFromVertex <$> topSort graph)

getStartLabels :: [SStmt] -> Edge
getStartLabels [SGoto l] = JumpEdge l
getStartLabels [SIf _ _ l1 l2] = BranchEdge l1 l2
getStartLabels (_:t) = getStartLabels t
getStartLabels [] = NoEdge

removeLoops :: SLabel -> RemoveLoopM ()
removeLoops l = do
    modify $ first (Set.insert l)
    (visited, edges) <- get
    let edge = edges ! l
    case edge of
        NoEdge -> return ()
        JumpEdge jl -> if Set.member jl visited then modify (second $ insert l NoEdge) else removeLoops jl
        BranchEdge l1 l2 -> case (Set.member l1 visited, Set.member l2 visited) of
            (True, True) -> modify (second $ insert l NoEdge)
            (True, False) -> modify (second $ insert l (JumpEdge l2)) >> removeLoops l2
            (False, True) -> modify (second $ insert l (JumpEdge l1)) >> removeLoops l1
            (False, False) -> removeLoops l1 >> removeLoops l2
