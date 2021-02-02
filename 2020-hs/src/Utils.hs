module Utils
  ( combinations,
    Solver,
  )
where

import Data.List (tails)

-- Return all subsets of length k of xs
combinations :: Int -> [a] -> [[a]]
combinations k xs
  | k == 0 = [[]]
  | otherwise =
    [x : ss | (x : rest) <- tails xs, ss <- combinations (k -1) rest]

type Solver = String -> [Int]
