{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thock where

import qualified Brick.Widgets.Edit as E
import Data.Function
import qualified Data.Text as T
import Data.Text.Zipper
import Lens.Micro
import Lens.Micro.TH

data Name = Thock deriving (Ord, Show, Eq)

data Game = Game
  { _prompt :: TextZipper T.Text,
    _input :: E.Editor T.Text Name
  }

makeLenses ''Game

progress :: Game -> Float
progress g = ((/) `on` fromIntegral) correct total
  where
    correct = col + sum (take row wordLengths) -- TODO: doesn't account for spaces
    total = sum wordLengths
    wordLengths = lineLengths promptCursor
    (row, col) = cursorPosition promptCursor
    promptCursor = g ^. prompt

movePromptCursor :: Game -> Game
movePromptCursor g = undefined

initializeGame :: Game
initializeGame =
  Game
    (textZipper ["Placeholder", "prompt!"] Nothing)
    (E.editor Thock (Just 1) "")
