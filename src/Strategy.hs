module Strategy where

import qualified Data.Set as S

import Data hiding (map)
import Graph

pickEdge :: Edges -> Maybe Edge
pickEdge es = headMaybe $ S.toList es

