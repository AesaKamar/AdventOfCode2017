{-# LANGUAGE FlexibleInstances #-}
module Lib where

import           Control.Applicative                  (many)
import           Control.Monad                        (replicateM)
import           Text.Parsec                          (ParseError, parse,
                                                       runParser, try)

import           Data.List
import           Data.Semigroup

import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.String                   (Parser)
import           Text.ParserCombinators.Parsec.Number

integer :: Parser Int
integer = read <$> many1 digit

nameParser :: Parser String
nameParser = many letter


weightParser :: Parser Int
weightParser = do
  _ <- char '('
  weight <- integer
  _ <- char ')'
  return weight


nameAndWeightParser :: Parser (String, Int)
nameAndWeightParser = do
  name <- nameParser
  _ <- space
  weight <- weightParser
  return (name, weight)



subTreeParser :: Parser [String]
subTreeParser = do
  _ <- space >> char '-' >> char '>'
  space
  nameWithSubtreeList <- nameParser `sepBy`  ( char ',' >> space)
  return nameWithSubtreeList

nameWeightAndSubtreesParser :: Parser (String, Int, Maybe [String])
nameWeightAndSubtreesParser = do
    nameAndWeight <- nameAndWeightParser
    theRest <- optionMaybe subTreeParser
    return (fst nameAndWeight, snd nameAndWeight, theRest)




data Tree = NonEmptyTree { name:: String, weight:: Int, subTrees:: [Tree]}
type TreeEntry = ( String, Int, Maybe [String])
data CoTreeEntry = CoTreeEntry  String (Maybe Int) (Maybe String) deriving (Show, Eq)

reverseTrees :: [TreeEntry] -> [CoTreeEntry]
reverseTrees = concatMap reverseTree

reverseTree :: TreeEntry -> [CoTreeEntry]
reverseTree (name, weight, Nothing) = [CoTreeEntry name (Just weight) Nothing]
reverseTree (name, weight, Just children) =
  fmap (\childName -> CoTreeEntry childName Nothing (Just name)) children

combineCoTrees :: Tree -> [CoTreeEntry] -> Tree
combineCoTrees origTree cots =
  let
    rootTree = head $ filter hasNoParent cots
    in foldl (\ tree coTree -> tree) origTree cots

insert :: Tree -> CoTreeEntry -> Tree
insert _ CoTreeEntry name maybeWeight Nothing = NonEmp

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile
