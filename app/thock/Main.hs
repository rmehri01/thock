module Main where

import qualified Brick.Main as M
import Thock (initialGame)
import UI.Offline (localApp)

main :: IO ()
main = do
  _ <- M.defaultMain localApp initialGame
  return ()
