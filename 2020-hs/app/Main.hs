module Main where

import Control.Monad (forM_)
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day07
import qualified Day12
import qualified Day13
import qualified Day18
import qualified Day19
import Text.Printf (printf)
import Utils (Solver)

solvers :: [(Int, Solver)]
solvers =
  [ (1, Day01.solve),
    (2, Day02.solve),
    (3, Day03.solve),
    (4, Day04.solve),
    (5, Day05.solve),
    (7, Day07.solve),
    (12, Day12.solve),
    (13, Day13.solve),
    (18, Day18.solve),
    (19, Day19.solve)
  ]

main :: IO ()
main =
  forM_
    solvers
    ( \(day, solver) -> do
        let fileName = printf "day%02d-input.txt" day
        let description = printf "Day %d" day
        input <- readFile fileName
        let answers = solver input
        putStrLn description
        forM_ (zip ([1 ..] :: [Int]) answers) (uncurry (printf "Part %d: %d\n"))
        putStrLn ""
    )
