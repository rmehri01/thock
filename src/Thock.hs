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
movePromptCursor g = movePromptByN move g
  where
    move = mostCorrect g - col
    (_, col) = cursorPosition (g ^. prompt)

movePromptByN :: Int -> Game -> Game
movePromptByN n g
  | n < 0 = movePromptByN (n + 1) (g & prompt %~ moveLeft)
  | n == 0 = g
  | n > 0 = movePromptByN (n - 1) (g & prompt %~ moveRight)

mostCorrect :: Game -> Int
mostCorrect g = length $ takeWhile (uncurry (==)) $ T.zip currentWord currentInput
  where
    currentInput = head $ E.getEditContents (g ^. input)
    currentWord = currentLine (g ^. prompt)

initializeGame :: Game
initializeGame =
  Game
    (textZipper ["Placeholder", "prompt!"] Nothing)
    (E.editor Thock (Just 1) "")
