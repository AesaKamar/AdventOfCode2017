{-# LANGUAGE TypeFamilies #-}


module Lib where

import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Prim
import           Text.Parsec.String

type RandomChars = String

data Input = Garbage
           | Group [Input]
           deriving (Show, Eq)



parserGarbage :: Parser Input
parserGarbage = do
  _ <- string "<"
  _ <- manyTill anyChar (string ">")
  return Garbage
