module Day13 where

import Data.List (find, maximumBy, minimumBy)
import Data.List.Utils (split)
import Utils (Solver)

-- Part 1: find lowest multiple of each bus id which is >= timestamp, and choose minimum
solvePart1 :: Int -> [(Int, Int)] -> Int
solvePart1 timestamp busIds =
  let busIdMultiples = [(id, (timestamp + id - 1) `div` id * id) | (_, id) <- busIds]
      (minId, departTime) = minimumBy (\(_, a) (_, b) -> compare a b) busIdMultiples
   in minId * (departTime - timestamp)

-- Answer from here: https://github.com/norvig/pytudes/blob/master/ipynb/Advent-2020.ipynb
solvePart2 :: [(Int, Int)] -> Int
solvePart2 busIds =
  let (time, step) =
        foldl
          ( \(time, step) (offset, busId) ->
              let loop time = if (time + offset) `mod` busId == 0 then time else loop (time + step)
               in (loop time, step * busId)
          )
          (0, 1)
          busIds
   in time

solve :: Solver
solve input =
  let [timestampStr, busIdsStr] = lines input
      timestamp = read timestampStr :: Int
      busIds =
        [ (index, read id)
          | (index, id) <- zip [0 ..] (split "," busIdsStr),
            id /= "x"
        ]

      part1 = solvePart1 timestamp busIds
      part2 = solvePart2 busIds
   in [part1, part2]
