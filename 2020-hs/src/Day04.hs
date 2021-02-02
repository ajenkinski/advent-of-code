-- https://adventofcode.com/2020/day/4

module Day04 where

import Data.Ix (inRange)
import Data.List (sort)
import Data.List.Utils (split)
import qualified Data.Map as Map
import Text.Regex.TDFA ((=~), (=~~))
import Utils (Solver)

type Passport = Map.Map String String

-- >>> parsePassport "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm"
-- fromList [("byr","1937"),("cid","147"),("ecl","gry"),("eyr","2020"),("hcl","#fffffd"),("hgt","183cm"),("iyr","2017"),("pid","860033327")]
parsePassport :: String -> Passport
parsePassport str =
  -- string can be spread over multiple lines.  Treat newlines and spaces the same and split into field strings
  let fields = [(k, v) | field <- words str, let [k, v] = split ":" field]
   in Map.fromList fields

requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

hasRequiredKeys :: Passport -> Bool
hasRequiredKeys passport = all (`Map.member` passport) requiredFields

type Validator = String -> Bool

-- >>> validateYear 2015 2020 "02020"
-- False
-- >>> validateYear 2015 2020 "2020"
-- True
-- >>> validateYear 2015 2020 "2010"
-- False
validateYear :: Int -> Int -> Validator
validateYear minYear maxYear str =
  str =~ "^[0-9]{4}$" && inRange (minYear, maxYear) (read str)

-- >>> validateHeight "10cm"
-- False
-- >>> validateHeight "150cm"
-- True
-- >>> validateHeight "60in"
-- True
validateHeight :: Validator
validateHeight str =
  Just True
    == do
      (_, _, _, [numStr, unit]) <- str =~~ "([0-9]+)(cm|in)" :: Maybe (String, String, String, [String])
      let num = read numStr
      let (lo, hi) = if unit == "in" then (59, 76) else (150, 193)
      return (inRange (lo, hi) num)

validators :: [(String, Validator)]
validators =
  [ ("byr", validateYear 1920 2002),
    ("iyr", validateYear 2010 2020),
    ("eyr", validateYear 2020 2030),
    ("hgt", validateHeight),
    ("hcl", (=~ "^#[a-f0-9]{6}$")),
    ("ecl", (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])),
    ("pid", (=~ "^[0-9]{9}$"))
  ]

hasValidKeys :: Passport -> Bool
hasValidKeys passport =
  all
    ( \(key, validator) ->
        case Map.lookup key passport of
          Just val -> validator val
          _ -> False
    )
    validators

solve :: Solver
solve input =
  let passports = map parsePassport (split "\n\n" input)
      part1 = length $ filter hasRequiredKeys passports
      part2 = length $ filter hasValidKeys passports
   in [part1, part2]