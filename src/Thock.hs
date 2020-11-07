{-# LANGUAGE OverloadedStrings #-}

module Thock where

import qualified Data.Text as T

data Game = Game
  { input :: T.Text,
    prompt :: T.Text
  }

initializeGame :: Game
initializeGame =
  Game
    { input = "",
      prompt = "Placeholder prompt!"
    }

