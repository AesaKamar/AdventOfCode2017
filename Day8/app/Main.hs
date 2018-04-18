module Main where


import           Control.Monad
import           Data.Foldable                       (foldl)
import           Data.Map.Strict                     as Map
import           Data.Traversable
import           Language.Haskell.HsColour
import           Language.Haskell.HsColour.Colourise
import           Lib
import           System.IO
import           Text.Parsec
import           Text.Show.Pretty

colorPrint = putStrLn . (hscolour TTY defaultColourPrefs False False "" False) . ppShow

main :: IO ()
main = do
  lns <- lines <$> readFile "./inputDay8"
  collected <- return $ statementFolderList lns
  result <- return $ combiningMapOperation <$> collected
  colorPrint result



combiningMapOperation mp =
  -- mp
  -- sum <$> mp
  sum . maxsubseq <$> mp
