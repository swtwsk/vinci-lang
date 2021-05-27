module SSA.Optimizations.SinglePhiElimination (eliminateSinglePhi) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (first, second)
import qualified Data.Map as Map
import qualified Data.Set as Set

import SSA.AST
import SSA.LabelGraph (BlocksMap)

type Visited = Set.Set SLabel
type EliminationM = State BlocksMap
type RemovePhiM   = ReaderT (VarName, VarName) (State (Visited, BlocksMap))

eliminateSinglePhi :: BlocksMap -> BlocksMap
eliminateSinglePhi blockMap = 
    execState (loopPhiRemoval $ Map.toList blockMap) blockMap

loopPhiRemoval :: [(SLabel, SLabelledBlock)] -> EliminationM ()
loopPhiRemoval ((h, _):t) = do
    hRemoved <- tryRemovePhi h
    if hRemoved then get >>= (loopPhiRemoval . Map.toList) else loopPhiRemoval t
loopPhiRemoval [] = return ()

tryRemovePhi :: SLabel -> EliminationM Bool
tryRemovePhi l = do
    blockMap <- get
    let (SLabelled _ phis b) = blockMap Map.! l
    case phis of
        ((SPhiNode v [(_, origVar)]):_) -> do
            let bSt  = (Set.empty, blockMap)
                bEnv = (origVar, _varName v)
                (b', (_, bm')) = 
                    runState (runReaderT (removePhiBlock b) bEnv) bSt
            put $ Map.insert l (SLabelled l [] b') bm'
            return True
        _ -> return False

removePhiLabelled :: SLabelledBlock -> RemovePhiM SLabelledBlock
removePhiLabelled (SLabelled l phiNodes b) = do
    modify $ first (Set.insert l)
    phis' <- mapM (\(SPhiNode v ps) -> SPhiNode v <$> clearPhiNode ps) phiNodes
    b' <- removePhiBlock b
    return $ SLabelled l phis' b'
    where
        clearPhiNode :: [(SLabel, String)] -> RemovePhiM [(SLabel, String)]
        clearPhiNode ((lx, x):t) = do
            (orig, toRemove) <- ask
            t' <- clearPhiNode t
            return $ if x == toRemove 
                then (lx, orig):t'
                else (lx, x):t'
        clearPhiNode [] = return []

removePhiBlock :: SBlock -> RemovePhiM SBlock
removePhiBlock (SBlock stmts) = SBlock <$> mapM removePhiStmt stmts

removePhiStmt :: SStmt -> RemovePhiM SStmt
removePhiStmt (SAssign v e) = SAssign v <$> removePhiExpr e
removePhiStmt (SGoto l) = do
    (visited, blocks) <- get
    let labelledBlock = blocks Map.! l
    unless (Set.member l visited) $ removePhiLabelled labelledBlock >>= \lb' ->
        modify $ second (Map.insert l lb')
    return (SGoto l)
removePhiStmt (SReturn e) = SReturn <$> removePhiExpr e
removePhiStmt (SIf sf e l1 l2) = do
    e' <- removePhiExpr e
    g1 <- removePhiStmt (SGoto l1)
    g2 <- removePhiStmt (SGoto l2)
    let (SGoto l1', SGoto l2') = (g1, g2)
    return $ SIf sf e' l1' l2'

removePhiExpr :: SExpr -> RemovePhiM SExpr
removePhiExpr (SVar v) = SVar <$> getVarAfterRemove v
removePhiExpr (SApp f vars) = do
    f' <- getVarAfterRemove f
    vars' <- mapM getVarAfterRemove vars
    return $ SApp f' vars'
removePhiExpr (SStructCtr sType vars) = 
    SStructCtr sType <$> mapM getVarAfterRemove vars
removePhiExpr (SStructGet i v) = SStructGet i <$> getVarAfterRemove v
removePhiExpr (STupleProj i v) = STupleProj i <$> getVarAfterRemove v
removePhiExpr (SBinOp op e1 e2) = 
    SBinOp op <$> removePhiExpr e1 <*> removePhiExpr e2
removePhiExpr (SUnOp op e) = SUnOp op <$> removePhiExpr e
removePhiExpr e = return e

getVarAfterRemove :: Var -> RemovePhiM Var
getVarAfterRemove (Var v t) = do
    (orig, toRemove) <- ask
    return $ Var (if v == toRemove then orig else v) t
