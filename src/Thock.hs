{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thock where

import qualified Brick.Widgets.Edit as E
import Data.Function
import qualified Data.Text as T
import Data.Text.Zipper
import Lens.Micro
import Lens.Micro.TH
import Quotes

data Name = Thock deriving (Ord, Show, Eq)

data Game = Game
  { _prompt :: TextZipper T.Text,
    _input :: E.Editor T.Text Name,
    _quote :: Quote
  }

makeLenses ''Game

progress :: Game -> Float
progress g = ((/) `on` fromIntegral) correct total
  where
    correct = numCorrectChars g
    total = g ^. (quote . numChars)

numCorrectChars :: Game -> Int
numCorrectChars g = correctBefore + col
  where
    correctBefore = T.length $ foldMap (`T.snoc` ' ') $ take row $ getText tz
    (row, col) = cursorPosition tz
    tz = g ^. prompt

movePromptCursor :: Game -> Game
movePromptCursor g =
  if currentWordFinished
    then g & prompt %~ moveRight & input %~ E.applyEdit clearZipper
    else g & prompt %~ movePromptByN moveAmount
  where
    currentWordFinished = currentInput == T.snoc currentWord ' '
    moveAmount = numCorrectCurrentWord g - col
    (_, col) = cursorPosition (g ^. prompt)
    currentInput = head $ E.getEditContents (g ^. input)
    currentWord = currentLine (g ^. prompt)

movePromptByN :: Int -> TextZipper T.Text -> TextZipper T.Text
movePromptByN n tz
  | n < 0 = movePromptByN (n + 1) (moveLeft tz)
  | n > 0 = movePromptByN (n - 1) (moveRight tz)
  | otherwise = tz

numCorrectCurrentWord :: Game -> Int
numCorrectCurrentWord g = length $ takeWhile (uncurry (==)) $ T.zip currentWord currentInput
  where
    currentWord = currentLine (g ^. prompt)
    currentInput = head $ E.getEditContents (g ^. input)

numIncorrectChars :: Game -> Int
numIncorrectChars g = T.length currentInput - numCorrectCurrentWord g
  where
    currentInput = head $ E.getEditContents (g ^. input)

initializeGame :: Quote -> Game
initializeGame q =
  Game
    { _prompt = textZipper (T.words (q ^. text)) Nothing,
      _input = E.editor Thock (Just 1) "",
      _quote = q
    }
