module Main where

import qualified Brick.Main as M
import Thock
import UI

main :: IO ()
main = do
  _ <- M.defaultMain theApp initialState
  return ()
