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
main
  -- contents <- readFile "./sampleinput.txt"
 = do
  lns <- lines <$> readFile "./day12input.txt"
  parsedFromTos <- return $ traverse (parse parseLine "") lns
  answer <- return $ fmap produceAnswers parsedFromTos
  putStrLn $ ppShow answer

produceAnswers :: [(Vertex, [Vertex])] -> (Int, Int)
produceAnswers fromTos =
  let edges = concat $ normalize <$> fromTos
      graph = buildG (0, length fromTos) edges
      numReachableFrom0 = length $ reachable graph 0
      numComponents = (length $ components graph) - 1
  in (numReachableFrom0, numComponents)
