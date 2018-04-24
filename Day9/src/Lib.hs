{-# LANGUAGE TypeFamilies #-}

module Lib where

import Data.Maybe (maybeToList)
import Data.Monoid
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String

type RandomChars = String

data Input
  = Garbage String
  | Group [Input]
  deriving (Show, Eq)

parserGarbage :: Parser Input
parserGarbage = do
  _ <- string "<"
  contents <- many normalChars
  _ <- string ">"
  return (Garbage $ concat contents)

normalChars :: Parser [Char]
normalChars = (many1 validChars) <|> try escapedPair
  where
    validChars =
      oneOf
        (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <>
         ['\'', '\"', '<', ' ', '{', '}', ','])
    specialChars = oneOf ['>', '!']
    escapedPair = do
      exclamation <- (char '!')
      following <- validChars <|> specialChars
      return [exclamation, following]

parseGroups :: Parser Input
parseGroups = do
  _ <- string "{"
  stuff <- (parseGroups <|> parserGarbage) `sepBy` string ","
  _ <- string "}"
  return (Group stuff)

countGroups :: Input -> Int
countGroups (Garbage _) = 0
countGroups (Group stuff) = 1 + sum (countGroups <$> stuff)

scoreGroups :: Int -> Input -> Int
scoreGroups level (Garbage _) = level
scoreGroups level (Group stuff) =
  level + sum ((scoreGroups (level + 1)) <$> stuff)
