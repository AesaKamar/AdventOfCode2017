{-# LANGUAGE TypeFamilies #-}

module Lib where

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

normalChars = (many1 validChars) <|> try (string "!>") <|> try (string "!!")
  where
    validChars =
      oneOf
        (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <>
         ['\'', '\"', '<', ' ', '{', '}'])
