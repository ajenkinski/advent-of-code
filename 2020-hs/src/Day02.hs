-- https://adventofcode.com/2020/day/2

module Day02 (solve) where

import Text.Regex.TDFA ((=~))
import Utils (Solver)
import Data.Bool

data Entry = Entry
  { num1, num2 :: Int,
    char :: Char,
    password :: String
  }
  deriving (Show)

-- >>> parseEntry "10-12 q: qqqqqqqqqqqdqqq"
-- Entry {num1 = 10, num2 = 12, char = 'q', password = "qqqqqqqqqqqdqqq"}
parseEntry :: String -> Entry
parseEntry line =
  let (_, _, _, [num1, num2, chr, password]) = line =~ "^([0-9]+)-([0-9]+) (.): (.+)$" :: (String, String, String, [String])
   in Entry (read num1) (read num2) (head chr) password

-- >>> part1Test (Entry 1 3 'a' "abca")
-- True
-- >>> part1Test (Entry 1 3 'a' "bcd")
-- False
-- >>> part1Test (Entry 1 3 'a' "abacadae")
-- False
part1Test :: Entry -> Bool
part1Test entry =
  let count = length [c | c <- password entry, c == char entry]
   in count >= num1 entry && count <= num2 entry

-- >>> part2Test (Entry 1 3 'a' "ab")
-- True
-- >>> part2Test (Entry 1 3 'a' "bcad")
-- True
-- >>> part2Test (Entry 1 3 'a' "bacd")
-- False
part2Test :: Entry -> Bool 
part2Test entry =
    let pw = password entry
        at1 = length pw >= num1 entry && pw!!(num1 entry - 1) == char entry
        at2 = length pw >= num2 entry && pw!!(num2 entry - 1) == char entry
    in (at1 || at2) && not (at1 && at2)


solve :: Solver
solve input =
    let entries = [parseEntry line | line <- lines input]
        part1 = length (filter part1Test entries)
        part2 = length (filter part2Test entries)
    in [part1, part2]
