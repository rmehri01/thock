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
movePromptCursor g =
  if currentWordFinished
    then g & prompt %~ moveRight & input %~ E.applyEdit clearZipper
    else g & prompt %~ movePromptByN moveAmount
  where
    currentWordFinished = currentInput == T.snoc currentWord ' '
    moveAmount = mostCorrect currentWord currentInput - col
    (_, col) = cursorPosition (g ^. prompt)
    currentInput = head $ E.getEditContents (g ^. input)
    currentWord = currentLine (g ^. prompt)

movePromptByN :: Int -> TextZipper T.Text -> TextZipper T.Text
movePromptByN n tz
  | n < 0 = movePromptByN (n + 1) (moveLeft tz)
  | n == 0 = tz
  | n > 0 = movePromptByN (n - 1) (moveRight tz)

mostCorrect :: T.Text -> T.Text -> Int
mostCorrect word = length . takeWhile (uncurry (==)) . T.zip word

initializeGame :: Game
initializeGame =
  Game
    (textZipper ["Placeholder", "prompt!"] Nothing)
    (E.editor Thock (Just 1) "")
