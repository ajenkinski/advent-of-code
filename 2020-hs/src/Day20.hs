{-# LANGUAGE TupleSections #-}

-- https://adventofcode.com/2020/day/20

module Day20 where

import Control.Monad (forM_, guard)
import Data.Foldable (find)
import Data.List (intersect, tails)
import Data.String.Utils (join, replace, split)
import Data.Map ((!), (!?))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (catMaybes, mapMaybe, maybe)
import System.IO.Unsafe (unsafePerformIO)
import Utils (Solver)

trace :: Show a => String -> a -> a
trace msg x = unsafePerformIO $ do
  print $ msg ++ show x
  return x

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

isOuterEdge :: EdgeCounts -> Edge -> Bool
isOuterEdge counts edge = (counts ! normalizeEdge edge) == 1

isCornerTile :: EdgeCounts -> Tile -> Bool
isCornerTile counts tile = length [e | e <- tileEdges tile, isOuterEdge counts e] == 2

solvePart1 :: TileMap -> Int
solvePart1 tiles =
  let counts = countEdges tiles
      -- This takes advantage of the fact that the problem definition states that outer edges won't match any other
      -- tiles.
      cornerIds = [id | (id, tile) <- Map.assocs tiles, isCornerTile counts tile]
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

data Problem = Problem
  { tiles :: TileMap,
    edgeMap :: Map.Map String [Int],
    edgeCounts :: EdgeCounts
  }
  deriving (Show)

parseProblem :: String -> Problem
parseProblem input =
  let tiles = parseTiles input
   in Problem
        { tiles = tiles,
          edgeMap = makeEdgeMap tiles,
          edgeCounts = countEdges tiles
        }

tilesWithEdges :: Problem -> [Edge] -> [IdTile]
tilesWithEdges prob edges =
  let tileIds = foldl1 intersect (map ((edgeMap prob !) . normalizeEdge) edges)
   in mapMaybe (\id -> fmap (id,) (tiles prob !? id)) tileIds

findCorners :: Problem -> [IdTile]
findCorners prob = [a | a@(_, tile) <- Map.assocs (tiles prob), isCornerTile (edgeCounts prob) tile]

assembleRow :: Problem -> Maybe Tile -> Maybe [Tile] -> [(Problem, [IdTile])]
assembleRow prob leftTile rowAbove = do
  -- If we run out of row above, fail
  guard (maybe True (not . null) rowAbove)

  let topEdge = tileBottom . head <$> rowAbove
  let leftEdge = tileRight <$> leftTile
  (nextId, nextTile) <- tilesWithEdges prob (catMaybes [leftEdge, topEdge])
  let prob' = prob {tiles = Map.delete nextId (tiles prob)}

  nextTransform <- transforms nextTile
  let nextLeft = map head nextTransform
  let nextTop = head nextTransform

  guard (maybe True (== nextLeft) leftEdge)
  guard (maybe True (== nextTop) topEdge)

  if isOuterEdge (edgeCounts prob') (tileRight nextTransform)
    then return (prob', [(nextId, nextTransform)])
    else do
      (prob', restRow) <- assembleRow prob' (Just nextTransform) (tail <$> rowAbove)
      return (prob', (nextId, nextTransform) : restRow)

assembleRows :: Problem -> Maybe [IdTile] -> [(Problem, [[IdTile]])]
assembleRows prob _ | Map.null (tiles prob) = [(prob, [])]
-- first row case
assembleRows prob@(Problem tiles edgeMap edgeCounts) Nothing =
  -- pick a corner, and find an orientation that puts the corner at the upper left
  let (cornerId, cornerTile) = head (findCorners prob)
      Just corner = find (\tile -> isOuterEdge edgeCounts (tileTop tile) && isOuterEdge edgeCounts (tileLeft tile)) (transforms cornerTile)
      prob' = prob {tiles = Map.delete cornerId tiles}
   in do
        (prob', firstRowRest) <- assembleRow prob' (Just corner) Nothing
        let firstRow = (cornerId, corner) : firstRowRest
        (prob', rest) <- assembleRows prob' (Just firstRow)
        return (prob', firstRow : rest)
assembleRows prob (Just prevRow) = do
  (prob', nextRow) <- assembleRow prob Nothing (Just $ map snd prevRow)
  (prob', rest) <- assembleRows prob' (Just nextRow)
  return (prob', nextRow : rest)

-- Assemble tiles in correct arrangement. Result is a list of lists, where each sublist is a row of (tileId, tile)
-- tuples, where tile is already flipped and rotated to fit.
assembleTiles :: Problem -> [[IdTile]]
assembleTiles prob =
  case assembleRows prob Nothing of
    (_, grid) : _ -> grid
    _ -> error "Couldn't find a solution"

-- Concatenate tiles into one big tile, after stripping off edges as required for part 2
concatTileCenters :: [[IdTile]] -> Tile
concatTileCenters grid =
  let tileCenter tile = map (init . tail) $ (init . tail) tile
      tileCenters = [map (tileCenter . snd) row | row <- grid]
   in [concat row | gridRow <- tileCenters, row <- zipMany gridRow]

-- Parse the monster pattern for part two into a regular expression.  It will match the monster assumming the grid
-- is concatenated into a single string with newlines separating rows
parsePattern :: Int -> String -> String
parsePattern gridWidth input = 
  let inputLines = filter (/= "") $ lines input 
      patternWidth = maximum $ map length inputLines
      patternChar ch = if ch == '#' then "[o#]" else "[^\n]"
      rowRxes = map (concatMap patternChar) inputLines
      filler = ".{" ++ show (gridWidth - patternWidth) ++ "}" 
   in join filler rowRxes

showGrid :: [[IdTile]] -> IO ()
showGrid grid =
  forM_ grid $ \gridRow ->
    do
      forM_ (zipMany (map snd gridRow)) $ \row ->
        putStrLn (join " " row)
      putStrLn ""

testAssembleTiles = do
  input <- readFile "day20-test-input.txt"
  let prob = parseProblem input
  let grid = assembleTiles prob
  showGrid grid
