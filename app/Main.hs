module Main where

import qualified Brick.Main as M
import Thock
import UI.Offline

main :: IO ()
main = do
  _ <- M.defaultMain theApp initialState
  return ()
