module Core.Toposort where

import Data.Graph
import qualified Data.Set as Set

import Core.AST
import Core.FreeVariables

type ProgNode a = (Prog a, String, [String])
data CoreGraph a = CoreGraph { _sorted         :: [Prog a]
                             , _graph          :: Graph
                             , _nodeFromVertex :: Vertex -> ProgNode a
                             , _vertexFromKey  :: String -> Maybe Vertex }

showGraph :: (ShowableFunctor f) => CoreGraph f -> String
showGraph coreGraph = unlines (show <$> _sorted coreGraph)

inverselySortTopologically :: [Prog a] -> [Prog a]
inverselySortTopologically = _sorted . inverselySortTopologicallyToGraph

inverselySortTopologicallyToGraph :: [Prog a] -> CoreGraph a
inverselySortTopologicallyToGraph progList = 
    CoreGraph { _sorted         = fstTriple . nodeFromVertex <$> sorted
              , _graph          = graph
              , _nodeFromVertex = nodeFromVertex
              , _vertexFromKey  = vertexFromKey }
    where
        nodes = map progToNode progList
        (graph, nodeFromVertex, vertexFromKey) = graphFromEdges nodes
        sorted = reverse $ topSort graph
        fstTriple (x, _, _) = x

progToNode :: Prog a -> ProgNode a
progToNode p@(Prog f _ _) = 
    (p, _varName f, _varName <$> Set.toList (freeVariablesProg p))
