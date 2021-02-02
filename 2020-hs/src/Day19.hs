module Day19 where

import Data.List.Utils (split)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, listToMaybe)
import Utils (Solver)

data Pattern
  = Const Char
  | Ref Int
  | Cat [Pattern]
  | Choice [Pattern] [Pattern]
  deriving (Show)

type PatternMap = Map.Map Int Pattern

parsePatterns :: [String] -> PatternMap
parsePatterns patternStrs = Map.fromList (map parsePattern patternStrs)

-- >>> parsePattern "42: \"a\""
-- (42,Const 'a')
-- >>> parsePattern "3: 42 43"
-- (3,Cat [Ref 42,Ref 43])
-- >>> parsePattern "4: 42 43 | 44 45"
-- (4,Choice [Ref 42,Ref 43] [Ref 44,Ref 45])
parsePattern :: String -> (Int, Pattern)
parsePattern patternLine =
  let [idStr, patStr] = split ": " patternLine
      pat = case split " | " patStr of
        ['"' : ch : '"' : _] -> Const ch
        [left, right] -> Choice (map (Ref . read) (words left)) (map (Ref . read) (words right))
        [nums] -> Cat (map (Ref . read) (words nums))
   in (read idStr, pat)

matchPattern :: [Pattern] -> String -> PatternMap -> Maybe String
matchPattern pattern msg patterns =
  case pattern of
    -- Pattern fully matched, return rest of message
    [] -> Just msg

    -- String consumed before pattern fully matched, match failed
    _ : _ | null msg -> Nothing

    -- Check next sub-pattern against start of message
    pat : rest ->
      case pat of
        Const ch | ch == head msg -> matchPattern rest (tail msg) patterns
        Const _ -> Nothing
        Ref id -> matchPattern ((fromJust $ Map.lookup id patterns) : rest) msg patterns
        Cat pats -> matchPattern (pats ++ rest) msg patterns
        Choice left right -> (listToMaybe . catMaybes) [matchPattern (pats ++ rest) msg patterns | pats <- [left, right]]

countMatches :: [String] -> PatternMap -> Int
countMatches msgs patterns =
  let Just pat0 = Map.lookup 0 patterns
      validMessage msg = matchPattern [pat0] msg patterns == Just ""
   in length (filter validMessage msgs)

solve :: Solver
solve input =
  let [patsStr, msgsStr] = split "\n\n" input
      pats = parsePatterns (lines patsStr)
      msgs = lines msgsStr

      part1Answer = countMatches msgs pats

      part2Updates =
        [ (8, Choice [Ref 42] [Ref 42, Ref 8]),
          (11, Choice [Ref 42, Ref 31] [Ref 42, Ref 11, Ref 31])
        ]
      part2Pats = Map.union (Map.fromList part2Updates) pats
      part2Answer = countMatches msgs part2Pats
   in [part1Answer, part2Answer]
