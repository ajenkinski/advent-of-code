-- https://adventofcode.com/2020/day/12

module Day12 where

import Utils (Solver)

data Part1State = Part1State
  { x, y, heading :: Int
  }
  deriving (Show)

data Part2State = Part2State
  { -- Ship x,y and waypoint x,y
    sx, sy, wx, wy :: Int
  }
  deriving (Show)

type Instruction = (Char, Int)

parseInstruction :: String -> Instruction
parseInstruction str = (head str, read (tail str))

degreesToRadians :: Int -> Float
degreesToRadians d = fromIntegral d * pi / 180

-- Rotate a point around the origin by angle (in degrees)
rotatePoint :: (Int, Int) -> Int -> (Int, Int)
rotatePoint (x, y) angle =
  let s = sin (degreesToRadians angle)
      c = cos (degreesToRadians angle)
   in ( round (fromIntegral x * c - fromIntegral y * s),
        round (fromIntegral y * c + fromIntegral x * s)
      )

execInstructionPart1 :: Part1State -> Instruction -> Part1State
execInstructionPart1 state (code, amount) =
  case code of
    'N' -> state {y = y state + amount}
    'S' -> state {y = y state - amount}
    'E' -> state {x = x state + amount}
    'W' -> state {x = x state - amount}
    'L' -> state {heading = (heading state - amount + 360) `mod` 360}
    'R' -> state {heading = (heading state + amount) `mod` 360}
    'F' ->
      let directionCode = ['N', 'E', 'S', 'W'] !! (heading state `div` 90)
       in execInstructionPart1 state (directionCode, amount)

-- >>> execInstructionPart2 (Part2State 0 0 1 0) ('L', 90)
-- Part2State {sx = 0, sy = 0, wx = 0, wy = 1}
-- >>> execInstructionPart2 (Part2State 0 0 1 0) ('R', 90)
-- Part2State {sx = 0, sy = 0, wx = 0, wy = -1}
execInstructionPart2 :: Part2State -> Instruction -> Part2State
execInstructionPart2 state (code, amount) =
  case code of
    'N' -> state {wy = wy state + amount}
    'S' -> state {wy = wy state - amount}
    'E' -> state {wx = wx state + amount}
    'W' -> state {wx = wx state - amount}
    'L' ->
      let (wx', wy') = rotatePoint (wx state, wy state) amount
       in state {wx = wx', wy = wy'}
    'R' ->
      let (wx', wy') = rotatePoint (wx state, wy state) (- amount)
       in state {wx = wx', wy = wy'}
    'F' ->
      state
        { sx = sx state + amount * wx state,
          sy = sy state + amount * wy state
        }

solve :: Solver
solve input =
  let instructions = map parseInstruction (lines input)
      initialPart1State = Part1State {x = 0, y = 0, heading = 90}
      part1State = foldl execInstructionPart1 initialPart1State instructions
      part1 = abs (x part1State) + abs (y part1State)

      initialPart2State = Part2State {sx = 0, sy = 0, wx = 10, wy = 1}
      part2State = foldl execInstructionPart2 initialPart2State instructions
      part2 = abs (sx part2State) + abs(sy part2State)
   in [part1, part2]
