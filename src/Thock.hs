{-# LANGUAGE TemplateHaskell #-}

module Thock where

import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Control.Applicative
import Data.Function
import qualified Data.Text as T
import Data.Text.Zipper
import Data.Time
import qualified Data.Vector as Vec
import Lens.Micro
import Lens.Micro.TH
import Quotes

data Game = Game
  { _prompt :: TextZipper T.Text,
    _input :: E.Editor T.Text (),
    _quote :: Quote,
    _start :: Maybe UTCTime,
    _lastUpdated :: Maybe UTCTime,
    _strokes :: Int
  }

makeLenses ''Game

type MenuList = L.List () T.Text

data GameState
  = MainMenu {_list :: MenuList}
  | Practice {_game :: Game}

makeLenses ''GameState

progress :: Game -> Float
progress g = ((/) `on` fromIntegral) correct total
  where
    correct = numCorrectChars g
    total = g ^. (quote . numChars)

numCorrectChars :: Game -> Int
numCorrectChars g = correctBefore + col
  where
    correctBefore = T.length . foldMap (`T.snoc` ' ') . take row $ getText tz
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
numCorrectCurrentWord g = length . takeWhile (uncurry (==)) $ T.zip currentWord currentInput
  where
    currentWord = currentLine (g ^. prompt)
    currentInput = head $ E.getEditContents (g ^. input)

numIncorrectChars :: Game -> Int
numIncorrectChars g = T.length currentInput - numCorrectCurrentWord g
  where
    currentInput = head $ E.getEditContents (g ^. input)

updateTime :: UTCTime -> Game -> Game
updateTime t g = g' & lastUpdated ?~ t
  where
    g' = case g ^. start of
      Nothing -> g & start ?~ t
      _ -> g

wpm :: Game -> Double
wpm g = if s == 0 then 0 else cps * (60 / 5)
  where
    cps = fromIntegral (numCorrectChars g) / s
    s = secondsElapsed g

accuracy :: Game -> Double
accuracy g = ((/) `on` fromIntegral) (g ^. (quote . numChars)) (g ^. strokes)

secondsElapsed :: Game -> Double
secondsElapsed g = maybe 0 realToFrac (liftA2 diffUTCTime (g ^. lastUpdated) (g ^. start))

isDone :: Game -> Bool
isDone g = numCorrectChars g == g ^. (quote . numChars)

initializeGame :: Quote -> Game
initializeGame q =
  Game
    { _prompt = textZipper (T.words (q ^. text)) Nothing,
      _input = E.editor () (Just 1) "",
      _quote = q,
      _start = Nothing,
      _lastUpdated = Nothing,
      _strokes = 0
    }

initialState :: GameState
initialState = MainMenu (L.list () (Vec.fromList ["Practice", "Online"]) 2)

startPracticeGame :: Quote -> GameState
startPracticeGame q = Practice {_game = initializeGame q}
