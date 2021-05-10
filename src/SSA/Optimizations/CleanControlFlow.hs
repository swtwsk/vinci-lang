{-# LANGUAGE LambdaCase #-}

module SSA.Optimizations.CleanControlFlow (
    cleanControlFlow, 
    countPrecedessors, 
    postOrder
) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (bimap, first)
import qualified Data.Map as Map

import SSA.AST
import SSA.LabelGraph

type CleanM = State (Edges, BlocksMap)

cleanControlFlow :: Edges -> BlocksMap -> (Edges, BlocksMap)
cleanControlFlow edges blockMap = execState clean (edges, blockMap)

clean :: CleanM ()
clean = do
    order <- gets (postOrder . fst)
    changed <- or <$> mapM cleanPass order
    when changed clean

cleanPass :: SLabel -> CleanM Bool
cleanPass label = do
    edge <- gets ((Map.! label) . fst)
    case edge of
        BranchEdge l1 l2 -> do
            let sameLabels = l1 == l2
            when sameLabels (redundantElimination label l1)
            return sameLabels
        JumpEdge target -> jumpElimination label target
        NoEdge -> return False

redundantElimination :: SLabel -> SLabel -> CleanM ()
redundantElimination label target = do
    (_, blocksMap) <- get
    let edgesFn = Map.insert label (JumpEdge target)
        (SLabelled l' phis (SBlock block)) = blocksMap Map.! label
        block' = changeLast block
        blocksFn = Map.insert label (SLabelled l' phis (SBlock block'))
    modify $ bimap edgesFn blocksFn
    where
        changeLast :: [SStmt] -> [SStmt]
        changeLast [_]   = [SGoto target]
        changeLast (h:t) = h:changeLast t
        changeLast [] = []

jumpElimination :: SLabel -> SLabel -> CleanM Bool
jumpElimination label target = do
    (edges, blocksMap) <- get
    let (SLabelled _ phis (SBlock block)) = blocksMap Map.! label
        isEmpty = null phis && block == [SGoto target]
    let (SLabelled _ tPhis (SBlock tBlock)) = blocksMap Map.! target
        isTargetEmptyBranching = null tPhis && 
            case tBlock of { [SIf {}] -> True; _ -> False }
        hasOnePrecedessor = null tPhis && (countPrecedessors edges target == 1)

    when isEmpty $ emptyElimination label target
    when (not isEmpty && hasOnePrecedessor) $ mergeBlocks label target
    when (not isEmpty && not hasOnePrecedessor && isTargetEmptyBranching) $
        hoistBlocks label target

    return $ isEmpty || hasOnePrecedessor || isTargetEmptyBranching

emptyElimination :: SLabel -> SLabel -> CleanM ()
emptyElimination label target = do
    edges <- gets fst
    let edgesFn = Map.map $ \case
            b@(BranchEdge l1 l2) -> 
                if l1 == label then BranchEdge target l2 else
                if l2 == label then BranchEdge l1 label else
                b
            j@(JumpEdge t) -> if t == label then JumpEdge target else j
            NoEdge -> NoEdge
        blocksMapFn = Map.map $ \(SLabelled l phis (SBlock b)) ->
            SLabelled l phis (SBlock $ changeLast b)
        labelPrecedessors = getPrecedessors edges label
        blocksMapFn' = flip Map.adjust target $ \(SLabelled l phis b) ->
            let phisFns = map (changePhi label) labelPrecedessors
                phis' = map (flip (foldr id) phisFns) phis in
            SLabelled l phis' b
    modify $ bimap (Map.delete label . edgesFn) (Map.delete label . blocksMapFn' . blocksMapFn)
    where
        changeLast :: [SStmt] -> [SStmt]
        changeLast [SGoto t] = [if t == label then SGoto target else SGoto t]
        changeLast [SIf sf e l1 l2] = (: []) $ let sif = SIf sf e in
            if l1 == label then sif target l2 else
            if l2 == label then sif l1 label else
            sif l1 l2
        changeLast (h:t) = h:changeLast t
        changeLast [] = []

mergeBlocks :: SLabel -> SLabel -> CleanM ()
mergeBlocks label target = do
    (edges, blocksMap) <- get
    let targetEdge = edges Map.! target
        edgesFn = Map.delete target . Map.insert label targetEdge
        (SLabelled _ lPhis (SBlock lBlock)) = blocksMap Map.! label
        (SLabelled _ tPhis (SBlock tBlock)) = blocksMap Map.! target
        block' = SLabelled label (lPhis ++ tPhis) $ SBlock (merge lBlock tBlock)
        fixPhisFn = Map.map $ \(SLabelled l phis b) ->
            SLabelled l (changePhi target label <$> phis) b
        blocksMapFn = fixPhisFn . Map.delete target . Map.insert label block'
    modify $ bimap edgesFn blocksMapFn
    where
        merge :: [SStmt] -> [SStmt] -> [SStmt]
        merge [SGoto _] stmts2 = stmts2
        merge (h:t) stmts2 = h:merge t stmts2
        merge [] stmts2 = stmts2

hoistBlocks :: SLabel -> SLabel -> CleanM ()
hoistBlocks label target = do
    (edges, blocksMap) <- get
    let targetEdge = edges Map.! target
        edgesFn = Map.insert label targetEdge
        (SLabelled _ lPhis (SBlock block)) = blocksMap Map.! label
        (SLabelled _ tPhis (SBlock tBlock)) = blocksMap Map.! target
        block' = changeLast block tBlock
        fixPhisFn = Map.map $ \(SLabelled l phis b) ->
            SLabelled l (changePhi target label <$> phis) b
        blocksMapFn = 
            Map.insert label (SLabelled label (lPhis ++ tPhis) (SBlock block'))
    modify $ bimap edgesFn (fixPhisFn . blocksMapFn)
    where
        changeLast :: [SStmt] -> [SStmt] -> [SStmt]
        changeLast [_] tJump = tJump
        changeLast (h:t) tJump = h:changeLast t tJump
        changeLast [] _ = []

countPrecedessors :: Edges -> SLabel -> Int
countPrecedessors edges label = length $ getPrecedessors edges label

getPrecedessors :: Edges -> SLabel -> [SLabel]
getPrecedessors edges label =
    let foldFn source edge acc = case edge of
            BranchEdge l1 l2 -> 
                if (l1 == label) || (l2 == label) then source:acc else acc
            JumpEdge t -> if t == label then source:acc else acc
            NoEdge -> acc
    in Map.foldrWithKey foldFn [] edges

changePhi :: SLabel -> SLabel -> SPhiNode -> SPhiNode
changePhi orig new (SPhiNode v blockVars) = SPhiNode v (changePhi' <$> blockVars)
    where
        changePhi' :: (SLabel, String) -> (SLabel, String)
        changePhi' = first (\l -> if l == orig then new else l)
