module Main where

import qualified Brick.Main as M
import Quotes
import Thock
import UI

main :: IO ()
main = do
  q <- generateQuote
  _ <- M.defaultMain theApp (initializeGame q)
  return ()
