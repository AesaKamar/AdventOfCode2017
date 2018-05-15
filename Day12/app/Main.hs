{-# LANGUAGE ApplicativeDo #-}
module Main where

import           Control.Monad.Trans
import           Data.Graph
import           Data.Traversable    (sequence)
import           Lib
import           System.IO
import           Text.Parsec
import           Text.Show.Pretty

main :: IO ()
main = do
  -- contents <- readFile "./sampleinput.txt"
  lns <- lines <$> readFile "./day12input.txt"
  parsedFromTos <- return $ sequence $ fmap (parse parseLine "") lns
  answer <- return $ fmap produceAnswers parsedFromTos
  putStrLn $ ppShow answer


produceAnswers :: [(Vertex, [Vertex])] -> (Int, Int)
produceAnswers fromTos =
  let
    edges = concat $ normalize <$> fromTos
    graph = buildG graphBound edges
    numReachableFrom0 = (length $ reachable graph 0) + 1
    numComponents = length $ components graph
    in (numReachableFrom0, numComponents)
