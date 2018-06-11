module Lib where

import qualified Data.Map.Strict                      as Map

import           Control.Monad
import           Data.Foldable
import           Data.Functor
import           Data.List                            (inits, maximumBy, tails)
import           Data.Monoid                          ((<>))
import           Data.Ord                             (comparing)
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.String
import           Text.ParserCombinators.Parsec.Number

type RegisterName = String

data Command
  = Increment Integer
  | Decrement Integer
  deriving (Show, Eq)

data Comparison
  = LessThan
  | LessThanOrEqualTo
  | GreaterThan
  | GreaterThanOrEqualTo
  | EqualTo
  | NotEqualTo
  deriving (Show, Eq)

type Predicate = (RegisterName, Comparison, Integer)

data Statement =
  Statement RegisterName
            Command
            Predicate
  deriving (Show, Eq)

-- Parses things of the form :
-- "inc -10"
-- "dec 4"
commandParser :: Parser Command
commandParser =
  Increment <$> (string "inc" >> space >> int) <|>
  Decrement <$> (string "dec" >> space >> int)

-- Parses things of the form :
-- "> 10"
-- "<= -12"
predicateParser :: Parser Predicate
predicateParser = do
  registerName <- many letter
  space
  comparison <-
    try (const GreaterThanOrEqualTo <$> string ">=") <|>
    (const GreaterThan <$> string ">") <|>
    try (const LessThanOrEqualTo <$> string "<=") <|>
    (const LessThan <$> string "<") <|>
    try (const EqualTo <$> string "==") <|>
    (const NotEqualTo <$> string "!=")
  space
  numToCompareTo <- int
  return (registerName, comparison, numToCompareTo)

-- Parses things of the form :
-- "b inc 5 if a > 1"
-- "c inc -20 if c == 10"
statementParser :: Parser Statement
statementParser = do
  registerName <- many letter
  space
  command <- commandParser
  space
  string "if"
  space
  predicate <- predicateParser
  return $ Statement registerName command predicate

-- Old part 1 stuff, only needed for testing
evaluate :: Map.Map RegisterName Integer -> Predicate -> Bool
evaluate mp (regName, cmp, comparisonVal) =
  let foundVal = Map.findWithDefault 0 regName mp
      isSomethingToFn =
        case cmp of
          GreaterThanOrEqualTo -> (>=)
          GreaterThan          -> (>)
          LessThanOrEqualTo    -> (<=)
          LessThan             -> (<)
          EqualTo              -> (==)
          NotEqualTo           -> (/=)
  in foundVal `isSomethingToFn` comparisonVal

statementInterpreter ::
     Map.Map RegisterName Integer -> Statement -> Map.Map RegisterName Integer
statementInterpreter m (Statement nameToAlter command predicate) =
  if evaluate m predicate
    then case command of
           Increment n -> Map.insertWith (+) nameToAlter n m
           Decrement n -> Map.insertWith (+) nameToAlter (negate n) m
    else m

statementFolder :: [String] -> Either ParseError (Map.Map RegisterName Integer)
statementFolder strings =
  let parsed = ((parse statementParser "") <$> strings)
      seqParsed = sequence parsed
      res =
        fmap
          (\x -> Data.Foldable.foldl statementInterpreter Map.empty x)
          seqParsed
  in res

-- PART 2 STUFF
evaluateList :: Map.Map RegisterName [Integer] -> Predicate -> Bool
evaluateList mp (regName, cmp, comparisonVal) =
  let foundVal = Map.findWithDefault [0] regName mp
      isSomethingToFn =
        case cmp of
          GreaterThanOrEqualTo -> (>=)
          GreaterThan          -> (>)
          LessThanOrEqualTo    -> (<=)
          LessThan             -> (<)
          EqualTo              -> (==)
          NotEqualTo           -> (/=)
  in (sum foundVal) `isSomethingToFn` comparisonVal

statementInterpreterList ::
     Map.Map RegisterName [Integer]
  -> Statement
  -> Map.Map RegisterName [Integer]
statementInterpreterList m (Statement nameToAlter command predicate) =
  if evaluateList m predicate
    then case command of
           Increment n -> Map.insertWith (++) nameToAlter [n] m
           Decrement n -> Map.insertWith (++) nameToAlter [negate n] m
    else m

statementFolderList ::
     [String] -> Either ParseError (Map.Map RegisterName [Integer])
statementFolderList strings =
  let parsed = ((parse statementParser "") <$> strings)
      seqParsed = sequence parsed
      res =
        fmap
          (\x -> Data.Foldable.foldl statementInterpreterList Map.empty x)
          seqParsed
  in res

subseqs :: [a] -> [[a]]
subseqs = tails

maxsubseq :: (Ord a, Num a) => [a] -> [a]
maxsubseq = maximumBy (comparing sum) . subseqs
  -- HELLO
