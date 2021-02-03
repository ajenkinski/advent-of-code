module Day18 where

import Control.Monad.Except (throwError)
import Control.Monad.State.Lazy (StateT, evalStateT, gets, state)
import qualified Data.Char as Char
import Data.List (span)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Utils (Solver)

type NumType = Int

data Token
  = Op Char
  | Lparen
  | Rparen
  | Number NumType
  deriving (Show)

data Ast
  = BinOp Ast Char Ast
  | Value NumType
  deriving (Show)

type PrecedenceMap = Map.Map Char Int

-- >>> tokenize "1 + (2 * 3)"
-- Right [Number 1,Op '+',Lparen,Number 2,Op '*',Number 3,Rparen]
-- >>> tokenize "1 x 2"
-- Left "Unrecognized character: 'x'"
tokenize :: String -> Either String [Token]
tokenize expr = sequence $ tokenize' expr
    where
        tokenize' :: String -> [Either String Token]
        tokenize' [] = []
        tokenize' expr@(ch : rest)
            | Char.isSpace ch = tokenize' rest
            | Char.isDigit ch =
                let (digits, rest) = span Char.isDigit expr
                in Right (Number (read digits)) : tokenize' rest
            | ch == '(' = Right Lparen : tokenize' rest
            | ch == ')' = Right Rparen : tokenize' rest
            | ch `elem` ['+', '-', '*', '/'] = Right (Op ch) : tokenize' rest
            | otherwise = [Left ("Unrecognized character: " ++ show ch)]

data ParserState = PS
  { tokens :: [Token],
    precMap :: PrecedenceMap
  }

-- Parser is a combination of the Either and State monads
type Parser = StateT ParserState (Either String)

getTokens :: Parser [Token]
getTokens = gets tokens

nextToken :: Parser Token
nextToken = do
  toks <- getTokens
  case toks of
    [] -> throwError "Unexpected end of expression"
    tok : rest -> state (\ps -> (tok, ps {tokens = rest}))

peekToken :: Parser (Maybe Token)
peekToken = listToMaybe <$> getTokens

getPrecedence :: Char -> Parser Int
getPrecedence op = do
  precs <- gets precMap
  return $ Map.findWithDefault 1 op precs

parseExpression :: Int -> Parser Ast
parseExpression precedence =
  do
    lhs <- parseOperand
    parseWhilePrecedenceHigher lhs
  where
    parseWhilePrecedenceHigher lhs =
      do
        next <- peekToken
        case next of
          Nothing -> return lhs
          Just Rparen -> return lhs
          Just (Op op) -> do
            opPrecedence <- getPrecedence op
            if opPrecedence <= precedence
              then return lhs
              else do
                nextToken
                rhs <- parseExpression opPrecedence
                parseWhilePrecedenceHigher $ BinOp lhs op rhs
          Just tok -> throwError $ "Unexpected token where operator expected: " ++ show tok

-- Parse either parenthesized expression or number
parseOperand :: Parser Ast
parseOperand = do
  tok <- nextToken
  case tok of
    Lparen -> do
      result <- parseExpression 0
      close <- nextToken
      case close of
        Rparen -> return result
        _ -> throwError "Missing expected ')'"
    Number n -> return $ Value n
    _ -> throwError $ "Unexpected token: " ++ show tok

-- >>> parseLine Map.empty "1 + (2 * 3)"
-- Right (BinOp (Value 1) '+' (BinOp (Value 2) '*' (Value 3)))
-- >>> parseLine Map.empty "1 +"
-- Left "Unexpected end of expression"
-- >>> parseLine Map.empty "1 + (2 3)"
-- Left "Unexpected token where operator expected: Number 3"
-- >>> parseLine (Map.fromList [('+', 2), ('*', 1)]) "1 * 2 + 3"
-- Right (BinOp (Value 1) '*' (BinOp (Value 2) '+' (Value 3)))
parseLine :: PrecedenceMap -> String -> Either String Ast
parseLine precMap line = do
    tokens <- tokenize line
    let parser = PS tokens precMap
    evalStateT (parseExpression 0) parser

evalExpression :: Ast -> Either String NumType
evalExpression expr =
  case expr of
    BinOp lhs op rhs -> do
      lhsVal <- evalExpression lhs
      rhsVal <- evalExpression rhs
      case op of
        '+' -> return (lhsVal + rhsVal)
        '-' -> return (lhsVal - rhsVal)
        '*' -> return (lhsVal * rhsVal)
        '/' -> return (lhsVal `div` rhsVal)
        _ -> throwError $ "Unknown operator: " ++ show op
    Value n -> Right n

evalExpressionString :: PrecedenceMap -> String -> Either String NumType
evalExpressionString precs expr =
  do
    ast <- parseLine precs expr
    evalExpression ast

solveProblem :: PrecedenceMap -> [String] -> NumType
solveProblem precs exprs =
  case mapM (evalExpressionString precs) exprs of
    Left errMsg -> error errMsg
    Right values -> sum values

solve :: Solver
solve input =
    let exprs = lines input
        part1Answer = solveProblem Map.empty exprs

        part2Answer = solveProblem (Map.fromList [('+', 2), ('*', 1)]) exprs
    in [part1Answer, part2Answer]
