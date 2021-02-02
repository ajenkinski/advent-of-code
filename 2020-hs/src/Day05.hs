-- https://adventofcode.com/2020/day/5

module Day05 where

import Utils (Solver)
import Data.List (tails, sort)

numRows = 128

numCols = 8

-- >>> computeSeatId "FBFBBFFRLR"
-- 357
-- >>> computeSeatId "BFFFBBFRRR"
-- 567
computeSeatId :: String -> Int
computeSeatId code =
  let (row, _, col, _) =
        foldl
          ( \(firstRow, lastRow, firstCol, lastCol) ch -> case ch of
              'F' -> (firstRow, lastRow - ((lastRow - firstRow + 1) `div` 2), firstCol, lastCol)
              'B' -> (firstRow + ((lastRow - firstRow + 1) `div` 2), lastRow, firstCol, lastCol)
              'L' -> (firstRow, lastRow, firstCol, lastCol - ((lastCol - firstCol + 1) `div` 2))
              'R' -> (firstRow, lastRow, firstCol + ((lastCol - firstCol + 1) `div` 2), lastCol)
          )
          (0, numRows - 1, 0, numCols - 1)
          code
   in row * numCols + col

solve :: Solver
solve input =
    let codes = lines input
        ids = map computeSeatId codes
        part1 = maximum ids
        -- Part 2 asks to find the gap in the sequence of ids
        part2 = head ([a + 1 | a:b:_ <- tails (sort ids), a + 1 /= b])
    in [part1, part2]
