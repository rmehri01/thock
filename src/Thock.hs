{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | This module provides the core data and functionality for handling the game.
module Thock where

import Brick (str)
import Brick.Forms (Form)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Control.Applicative (Applicative (liftA2))
import Control.Lens
  ( makeFieldsNoPrefix,
    makeLenses,
    (%~),
    (&),
    (?~),
    (^.),
  )
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAlphaNum, isAscii)
import Data.Function (on)
import qualified Data.Text as T
import Data.Text.Zipper
  ( TextZipper,
    clearZipper,
    currentLine,
    cursorPosition,
    getText,
    moveLeft,
    moveRight,
    textZipper,
  )
import Data.Time (UTCTime, diffUTCTime)
import qualified Data.Vector as Vec
import GHC.Generics (Generic)
import Quotes (Quote, QuotesSet (..), numChars, text)
import System.Random (Random (randoms), getStdGen)

-- | Unique identifiers to describe cursor locations
data ResourceName
  = UsernameField
  | RoomIdField
  | Ordinary
  deriving (Eq, Ord, Show)

-- | The state that tracks a player's progress
data GameState = GameState
  { -- | Current word the player is on, as well as previous and future words
    _prompt :: TextZipper T.Text,
    -- | Player input for the current word
    _input :: E.Editor T.Text ResourceName,
    -- | The set of choosen quotes
    _quotesSet :: QuotesSet,
    -- | The quote being typed
    _quote :: Quote,
    -- | The time the game started if it has started
    _start :: Maybe UTCTime,
    -- | The last time the game was updated if it has started
    _lastUpdated :: Maybe UTCTime,
    -- | The number of keyboard strokes made
    _strokes :: Int
  }

makeLenses ''GameState

type MenuList = L.List ResourceName T.Text

newtype Username = Username {_value :: T.Text}
  deriving (Generic)

makeLenses ''Username

instance FromJSON Username

instance ToJSON Username

type RoomId = T.Text

-- | The data needed to join an online room
data RoomFormData = RoomFormData
  { _username :: Username,
    _roomId :: RoomId
  }
  deriving (Generic)

makeFieldsNoPrefix ''RoomFormData

instance FromJSON RoomFormData

instance ToJSON RoomFormData

type RoomForm a = Form a () ResourceName

data RoomInitData = RoomInitData
  { _uname :: Username,
    _qset :: QuotesSet
  }

makeFieldsNoPrefix ''RoomInitData

-- | The current status of the game
data Game
  = MainMenu MenuList
  | PracticeSelectLang MenuList
  | OnlineSelect MenuList
  | OnlineSelectLang MenuList
  | CreateRoomMenu (RoomForm RoomInitData)
  | JoinRoomMenu (RoomForm RoomFormData)
  | Practice GameState
  | ErrorOverlay Game T.Text

makeLenses ''Game

-- | Produces the decimal amount the player has completed correctly
calculateProgress :: GameState -> Float
calculateProgress g = ((/) `on` fromIntegral) correct total
  where
    correct = numCorrectChars g
    total = g ^. (quote . numChars)

-- | Produces the total number of correct characters the player has typed
numCorrectChars :: GameState -> Int
numCorrectChars g = correctBefore + col
  where
    correctBefore = T.length . foldMap (`T.snoc` ' ') . take row $ getText tz
    (row, col) = cursorPosition tz
    tz = g ^. prompt

-- | Updates the prompt to be in sync with the user's input
movePromptCursor :: GameState -> GameState
movePromptCursor g =
  if currentWordFinished
    then g & prompt %~ moveRight & input %~ E.applyEdit clearZipper -- move onto the next word and clear the input box
    else g & prompt %~ movePromptByN moveAmount
  where
    currentWordFinished = currentInput == T.snoc currentWord ' '
    moveAmount = numCorrectCurrentWord g - col
    (_, col) = cursorPosition (g ^. prompt)
    currentInput = head $ E.getEditContents (g ^. input)
    currentWord = currentLine (g ^. prompt)

-- | Moves the prompt by n spaces right if positive and left if negative
movePromptByN :: Int -> TextZipper T.Text -> TextZipper T.Text
movePromptByN n tz
  | n < 0 = movePromptByN (n + 1) (moveLeft tz)
  | n > 0 = movePromptByN (n - 1) (moveRight tz)
  | otherwise = tz

-- | Produces the number of correct characters the player has typed in the current word
numCorrectCurrentWord :: GameState -> Int
numCorrectCurrentWord g = length . takeWhile (uncurry (==)) $ T.zip currentWord currentInput
  where
    currentWord = currentLine (g ^. prompt)
    currentInput = head $ E.getEditContents (g ^. input)

-- | Produces the number of incorrect characters the player has typed
numIncorrectChars :: GameState -> Int
numIncorrectChars g = T.length currentInput - numCorrectCurrentWord g
  where
    currentInput = head $ E.getEditContents (g ^. input)

-- | Starts the timer if it hasn't started already and sets lastUpdated to t
updateTime :: UTCTime -> GameState -> GameState
updateTime t g = g' & lastUpdated ?~ t
  where
    g' = case g ^. start of
      Nothing -> g & start ?~ t
      _ -> g

-- | Calculates typing speed in words per minute where a word is 5 characters
calculateWpm :: GameState -> Double
calculateWpm g = if s == 0 then 0 else cps * (60 / 5)
  where
    cps = fromIntegral (numCorrectChars g) / s
    s = secondsElapsed g

-- | Calculates decimal amount of correct typed / total typed characters
accuracy :: GameState -> Double
accuracy g = ((/) `on` fromIntegral) (g ^. (quote . numChars)) (g ^. strokes)

-- | The number of seconds from start to lastUpdated
secondsElapsed :: GameState -> Double
secondsElapsed g = maybe 0 realToFrac (liftA2 diffUTCTime (g ^. lastUpdated) (g ^. start))

-- | Produces true if the game is done, false otherwise
isDone :: GameState -> Bool
isDone g = numCorrectChars g == g ^. (quote . numChars)

-- | Creates an initial game with the given quote
initializeGameState :: QuotesSet -> Quote -> GameState
initializeGameState s q =
  GameState
    { _prompt = textZipper (T.words (q ^. text)) Nothing,
      _input = E.editor Ordinary (Just 1) "",
      _quotesSet = s,
      _quote = q,
      _start = Nothing,
      _lastUpdated = Nothing,
      _strokes = 0
    }

-- | Creates an initial 'Game' starting at the main menu
initialGame :: Game
initialGame = MainMenu (L.list Ordinary (Vec.fromList ["Practice", "Online"]) 2)

-- | Creates a select `QuotesSet` menu after choosing practice
pracSelectLang :: Game
pracSelectLang = PracticeSelectLang langList

-- | Initializes a 'Practice' game with the given quote
startPracticeGame :: QuotesSet -> Quote -> Game
startPracticeGame qs q = Practice $ initializeGameState qs q

-- | Creates a 'Game' for the online select menu with options to create or join a room
onlineSelectState :: Game
onlineSelectState = OnlineSelect (L.list Ordinary (Vec.fromList ["Create room", "Join room"]) 2)

-- | Creates a `QuotesSet` menu after choosing online game
onlineSelectLang :: Game
onlineSelectLang = OnlineSelectLang langList

-- | Creates a `QuotesSet` menu
langList :: L.GenericList ResourceName Vec.Vector T.Text
langList = L.list Ordinary (Vec.fromList ["English", "Russian", "Portuguese", "Haskell"]) 4

-- | Randomly generates an alphanumeric 'RoomId'
generateRoomId :: IO RoomId
generateRoomId = T.pack . take 10 . filter (\c -> isAscii c && isAlphaNum c) . randoms <$> getStdGen
