module Core.Toposort where

import Data.Graph
import qualified Data.Set as Set

import Core.AST
import Core.FreeVariables

type BindingNode a = (Binding a, String, [String])
data CoreGraph a = CoreGraph { _sorted         :: [Binding a]
                             , _graph          :: Graph
                             , _nodeFromVertex :: Vertex -> BindingNode a
                             , _vertexFromKey  :: String -> Maybe Vertex }

showGraph :: (ShowableFunctor f) => CoreGraph f -> String
showGraph coreGraph = unlines (show <$> _sorted coreGraph)

inverselySortTopologically :: [Binding a] -> [Binding a]
inverselySortTopologically = _sorted . inverselySortTopologicallyToGraph

inverselySortTopologicallyToGraph :: [Binding a] -> CoreGraph a
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

progToNode :: Binding a -> BindingNode a
progToNode pb@(ProgBinding p@(Prog f _ _)) = 
    (pb, _varName f, _varName <$> Set.toList (freeVariablesProg p))
progToNode c@(ConstBinding var _) = (c, _varName var, [])
