module Main where


import           Data.List
import           Lib2
import           System.IO

separateby :: Eq a => a -> [a] -> [[a]]
separateby tok = unfoldr sep where
  sep [] = Nothing
  sep xs = Just . fmap (drop 1) . break (== tok) $ xs


main = do
  contents <- readFile "./inputDay11.txt"
  tokens <- return $ parse <$>  separateby ',' (init contents)
  return $ distance mempty ( foldMap move tokens)
