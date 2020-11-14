module Main where

import UI
import Thock
import Quotes
import qualified Brick.Main as M

main :: IO ()
main = do
  quote <- generateQuote
  _ <- M.defaultMain theApp (initializeGame quote)
  return ()
