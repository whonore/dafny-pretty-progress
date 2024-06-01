module Main (main) where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Prettify

main :: IO ()
main = TIO.interact $ T.unlines . prettify . T.lines
