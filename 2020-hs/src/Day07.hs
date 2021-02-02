-- https://adventofcode.com/2020/day/7

module Day07 (solve) where

import qualified Data.Graph as G
import Data.List.Utils (split)
import qualified Data.Map as Map
import Text.Regex.TDFA ((=~))
import Utils (Solver)

-- Represent input as a directed graph with weighted edges.  Node keys are the bag colors, edge weights are required
-- number of bags.  Node A has an edge to B if bag color A contains one or more Bs.  G.Graph doesn't directly support edge weights,
-- but it does support arbitrary node metadata, so I store a map from neighbor key to weight as the node data.
data Bags = Bags
  { graph :: G.Graph,
    nodeFromVertex :: G.Vertex -> (Map.Map String Int, String, [String]),
    vertexFromKey :: String -> Maybe G.Vertex
  }

-- Parse line into (bagColor, [(childColor, childCount)]) tuple
parseLine :: String -> (String, [(String, Int)])
parseLine line =
  let [color, nbrsStr] = split " bags contain " line
      nbrStrs = split ", " nbrsStr
      nbrStrMatches = map (\s -> s =~ "([0-9]+) (.+) bag" :: (String, String, String, [String])) nbrStrs
      nbrs = [(nbrColor, read nbrCount) | (_, _, _, [nbrCount, nbrColor]) <- nbrStrMatches]
   in (color, nbrs)

parseInput :: String -> Bags
parseInput input =
  let parsed = map parseLine (lines input)
      adjacencyList = [(Map.fromList nbrs, color, map fst nbrs) | (color, nbrs) <- parsed]
      (graph, nodeFromVertex, vertexFromKey) = G.graphFromEdges adjacencyList
   in Bags graph nodeFromVertex vertexFromKey

countBags :: Bags -> String -> Int
countBags bags key =
  let Just vert = vertexFromKey bags key
      (counts, _, _) = nodeFromVertex bags vert
   in sum [numBags + numBags * countBags bags nbr | (nbr, numBags) <- Map.assocs counts]

solve :: Solver
solve input =
  let bags = parseInput input
      Just shinyGoldId = vertexFromKey bags "shiny gold"
      -- For part 1 we want to find how many bags contain a shiny gold bag.  In graph terms, count the ancestors
      -- subtract 1 from length because reachable counts the source node as reachable.
      part1 = length (G.reachable (G.transposeG $ graph bags) shinyGoldId) - 1
      part2 = countBags bags "shiny gold"
   in [part1, part2]
