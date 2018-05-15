{-# LANGUAGE FlexibleContexts #-}
module Lib where


import           Data.Array
import           Data.Graph


import           Control.Monad          (ap, liftM)
import           Data.Char              (digitToInt)
import           ParsecNum
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Prim
import           Text.Parsec.String





graphBound :: Bounds
graphBound = (0, 1999)



parseLine :: Parser (Vertex, [Vertex])
parseLine = do
  from <- int
  space >> string "<->" >> space
  tos <- int `sepBy` (string ", ")
  return (from, tos)


normalize :: (Vertex, [Vertex]) -> [(Vertex, Vertex)]
normalize (from, tos) = (\to -> (from, to)) <$> tos
