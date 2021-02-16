{-# LANGUAGE TupleSections #-}

-- https://adventofcode.com/2020/day/20

module Day20 where

import Control.Monad (forM_, guard)
import Data.Foldable (find)
import Data.List (intersect, tails)
import Data.List.Utils (join, split)
import Data.Map ((!), (!?))
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe, maybe)
import Utils (Solver)

type Edge = String

type Tile = [String]

type IdTile = (Int, Tile)

type TileMap = Map.Map Int Tile

showIdTile :: IdTile -> String
showIdTile (tileId, tile) = "Tile " ++ show tileId ++ ":\n" ++ join "\n" tile

solve :: Solver
solve input =
  let tiles = parseTiles input
      part1Answer = solvePart1 tiles
   in [part1Answer]

parseTiles :: String -> TileMap
parseTiles input =
  let tiles = filter (/= "") $ split "\n\n" input
      idTiles = [(read n :: Int, tail rows) | tile <- tiles, let rows = lines tile, let n = words (init (head rows)) !! 1]
   in Map.fromList idTiles

-- Part 1.  We just need to find which tiles are the corner tiles.  It turns out that because problem description states
-- that outer edges won't match any other edge, we can find the corner tiles without actually needing to assemble all
-- the tiles, by just finding the tiles with two unique edges.

normalizeEdge :: Edge -> Edge
normalizeEdge edge = min edge (reverse edge)

tileTop :: Tile -> Edge
tileTop = head

tileBottom :: Tile -> Edge
tileBottom = last

tileLeft :: Tile -> Edge
tileLeft = map head

tileRight :: Tile -> Edge
tileRight = map last

-- Return the 4 edges of the tile, in the order top, bottom, left and right
tileEdges :: Tile -> [Edge]
tileEdges tile = [tileTop tile, tileBottom tile, tileLeft tile, tileRight tile]

-- Create a map, where keys are normalized edges, and values are a list of the ids of tiles that have that edge
makeEdgeMap :: TileMap -> Map.Map String [Int]
makeEdgeMap tiles =
  Map.fromListWith (++) [(normalizeEdge edge, [id]) | (id, tile) <- Map.assocs tiles, edge <- tileEdges tile]

type EdgeCounts = Map.Map Edge Int

-- Returns a map from normalized edge, to how many times that edge occurs in the set of tiles
countEdges :: TileMap -> EdgeCounts
countEdges tiles = fmap length (makeEdgeMap tiles)

isCorner :: EdgeCounts -> Tile -> Bool
isCorner counts tile =
  let isOuter edge = (counts ! normalizeEdge edge) == 1
   in length [e | e <- tileEdges tile, isOuter e] == 2

solvePart1 :: TileMap -> Int
solvePart1 tiles =
  let counts = countEdges tiles
      -- This takes advantage of the fact that the problem definition states that outer edges won't match any other
      -- tiles.
      cornerIds = [id | (id, tile) <- Map.assocs tiles, isCorner counts tile]
   in product cornerIds

-- Part 2.  In part 1 we could get away with not actually assembling the tiles, by just finding which tiles were the
-- corners. For part 2 we actually need to assemble the tiles.

-- generalized zip for any number of input lists.
zipMany :: [[a]] -> [[a]]
zipMany lists
  | null lists = []
  | any null lists = []
  | otherwise = map head lists : zipMany (map tail lists)

-- Rotate tile given number of degrees
rotateTile :: Int -> Tile -> Tile
rotateTile angle tile =
  case angle `mod` 360 of
    0 -> tile
    90 -> zipMany tile
    180 -> reverse (map reverse tile)
    270 -> zipMany (map reverse tile)
    _ -> error "Angle must be a multiple of 90"

-- Flip tile over along vertical axis
flipTile :: Tile -> Tile
flipTile = map reverse

-- Given a tile, return all transformations (rotation and flips) including the input tile
transforms :: Tile -> [Tile]
transforms tile = [f (rotateTile angle tile) | angle <- [0, 90, 180, 270], f <- [id, flipTile]]

-- Assemble tiles in correct arrangement. Result is a list of lists, where each sublist is a row of (tileId, tile)
-- tuples, where tile is already flipped and rotated to fit.
assembleTiles :: TileMap -> [[IdTile]]
assembleTiles tiles =
  case assembleRows tiles Nothing of
    (_, grid) : _ -> grid
    _ -> error "Couldn't find a solution"
  where
    edgeMap = makeEdgeMap tiles
    edgeCounts = countEdges tiles
    corners = [pair | pair@(_, tile) <- Map.assocs tiles, isCorner edgeCounts tile]

    -- Return all tiles that cointain all the specified edges
    tilesWithEdges :: TileMap -> [Edge] -> [IdTile]
    tilesWithEdges tiles edges =
      let tileIds = foldl1 intersect (map (edgeMap !) edges)
       in mapMaybe (\id -> fmap (id,) (tiles !? id)) tileIds

    -- Return all possible rows whose leftMost tile starts with leftEdge, and optionally matching rowAbove
    -- It's an error if both leftEdge and rowAbove are Nothing
    assembleRow :: TileMap -> Maybe Edge -> Maybe [Tile] -> [(TileMap, [IdTile])]
    assembleRow tiles leftEdge rowAbove = do
      -- If we run out of row above, fail
      guard (maybe True (not . null) rowAbove)

      let topEdge = tileBottom . head <$> rowAbove
      (nextId, nextTile) <- tilesWithEdges tiles (catMaybes [normalizeEdge <$> leftEdge, topEdge])
      let tiles' = Map.delete nextId tiles

      nextTransform <- transforms nextTile
      let nextLeft = map head nextTransform
      let nextTop = head nextTransform

      guard (maybe True (== nextLeft) leftEdge)
      guard (maybe True (== nextTop) topEdge)

      if isCorner edgeCounts nextTransform
        then return (tiles', [(nextId, nextTransform)])
        else do
          (tiles', restRow) <- assembleRow tiles' (Just (tileRight nextTransform)) (tail <$> rowAbove)
          return (tiles', (nextId, nextTransform) : restRow)

    assembleRows :: TileMap -> Maybe [IdTile] -> [(TileMap, [[IdTile]])]
    assembleRows tiles _ | Map.null tiles = []
    -- first row case
    assembleRows tiles Nothing =
      -- pick a corner, and find an orientation that puts the corner at the upper left
      let (cornerId, cornerTile) = head corners
          Just corner = find (\tile -> (edgeCounts ! tileTop tile) == 1 && (edgeCounts ! tileLeft tile) == 1) (transforms cornerTile)
          tiles' = Map.delete cornerId tiles
       in do
            (tiles', firstRow) <- assembleRow tiles' (Just (tileRight corner)) Nothing
            (tiles', rest) <- assembleRows tiles' (Just firstRow)
            return (tiles', firstRow : rest)
    assembleRows tiles (Just prevRow) = do
      (tiles', nextRow) <- assembleRow tiles Nothing (Just $ map snd prevRow)
      (tiles', rest) <- assembleRows tiles' (Just nextRow)
      return (tiles', nextRow : rest)

showGrid :: [[IdTile]] -> IO ()
showGrid grid =
  forM_ grid $ \gridRow ->
    do 
      forM_ (zipMany (map snd gridRow)) $ \row ->
        putStrLn (join " " row)
      putStrLn ""
