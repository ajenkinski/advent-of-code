-- https://adventofcode.com/2020/day/2
module Day01 where

import Data.List (find)
import Data.Maybe (fromJust)
import Utils (Solver, combinations)

add a b = a + b

solve :: Solver
solve input =
  let nums = [read line | line <- lines input]
      part1 =
        let [a, b] = fromJust (find (\[a, b] -> a + b == 2020) (combinations 2 nums))
         in a * b
      part2 =
        let [a, b, c] = fromJust (find (\[a, b, c] -> a + b + c == 2020) (combinations 3 nums))
         in a * b * c
   in [part1, part2]
