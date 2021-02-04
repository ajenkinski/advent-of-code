module Day18Parsec where

import Control.Applicative (Alternative ((<|>)))
import Data.Functor (($>))
import Text.Parsec (Parsec, between, many1, parse)
import qualified Text.Parsec.Char as PC
import qualified Text.Parsec.Expr as PE
import Utils (Solver)

type NumType = Int

type Parser = Parsec String ()

-- Applies parser p, then consumes whitespace, and returns the value of p
lexeme :: Parser a -> Parser a
lexeme p = p <* PC.spaces

parens :: Parser a -> Parser a
parens = lexeme . between (PC.char '(') (PC.char ')')

natural :: Parser NumType
natural = lexeme (read <$> many1 PC.digit)

operator :: String -> Parser String
operator op = lexeme (PC.string op)

-- A table of binary operators in descending precedences.  Operators in one list are the same precedence.
-- Each operator is a (name, func) tuple, where func is the implementation for name.
type PrecTable a = [[(String, a -> a -> a)]]

makeParser :: Parser a -> PrecTable a -> Parser a
makeParser numParser precedences =
  let table = [[PE.Infix (operator op $> fun) PE.AssocLeft | (op, fun) <- level] | level <- precedences]
      term = parens expr <|> numParser
      expr = PE.buildExpressionParser table term
   in expr

solveAll :: PrecTable NumType -> [String] -> NumType
solveAll precs exprs =
  let parser = makeParser natural precs
      parsed = sequence [parse parser expr expr | expr <- exprs]
   in case parsed of
        Right nums -> sum nums
        Left err -> error (show err)

solvePart1 :: [String] -> NumType
solvePart1 = solveAll [[("+", (+)), ("*", (*))]]

solvePart2 :: [String] -> NumType
solvePart2 = solveAll [[("+", (+))], [("*", (*))]]

solve :: Solver
solve input =
  let exprs = lines input
      part1Answer = solvePart1 exprs
      part2Answer = solvePart2 exprs
   in [part1Answer, part2Answer]
