module Graph where

import qualified Data.Set as S

import Data hiding (map)

type Edge  = (Int, Int) -- require first index < second index
type Edges = S.Set Edge

claimMoveToEdge :: ClaimMove -> Edge
claimMoveToEdge (ClaimMove _ s t) = if s < t then (s,t) else (t,s)

edgeToClaimMove :: Int -> Edge -> ClaimMove
edgeToClaimMove punterId (s , t) = ClaimMove punterId s t

riverToEdge :: River -> Edge
riverToEdge (River s t) = if s < t then (s,t) else (t,s)

edgeToRiver :: Edge -> River
edgeToRiver (s,t) = River s t

riversToEdges :: [River] -> Edges
riversToEdges rs = S.fromList (map riverToEdge rs)

moveToEdges :: Move -> [Edge]
moveToEdges (Pass _) = []
moveToEdges (Claim cm) = [claimMoveToEdge cm]

serverMoveToEdges :: ServerMove -> Edges
serverMoveToEdges (ServerMove ms) = S.fromList (ms >>= moveToEdges)

--------

headMaybe :: [a] -> Maybe a
headMaybe []      = Nothing
headMaybe (x : _) = Just x

removeEdges :: Edges -> Edges -> Edges
removeEdges es es' = es S.\\ es'

--------

type Node = Int
type Nodes = S.Set Node

nodes :: Edges -> Nodes
nodes = S.foldl' (\s (a, b) -> S.insert a (S.insert b s)) S.empty

inEdge :: Node -> Edge -> Bool
inEdge n (a,b) = if n == a || n == b then True else False

adjacentEdges :: Nodes -> Edges -> Edges
adjacentEdges ns es = S.fromList $ (S.toList ns) >>= (\n -> S.toList $ S.filter (inEdge n) es)

shortestPathStep :: Nodes -> Edges -> (Nodes, Edges)
shortestPathStep ns es = let es' = adjacentEdges ns es in (nodes es', es S.\\ es')
