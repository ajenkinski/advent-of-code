-- https://adventofcode.com/2020/day/3

module Day03 (solve) where

import Utils

numTreesForSlope :: [String] -> Int -> Int -> Int
numTreesForSlope rows slopeRight slopeDown =
  let indexes = zip [0, slopeDown .. length rows - 1] [0, slopeRight ..]
      hasTree (rowNum, colNum) = let row = rows !! rowNum in row !! (colNum `mod` length row) == '#'
   in length $ filter hasTree indexes

solve :: Solver
solve input =
  let rows = lines input
      part1 = numTreesForSlope rows 3 1
      part2 =
        let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
         in product [numTreesForSlope rows r c | (r, c) <- slopes]
   in [part1, part2]
