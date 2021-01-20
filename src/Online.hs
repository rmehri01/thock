{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module has shared types and functions for communication between the websocket client and server.
module Online where

import Control.Lens (makeFieldsNoPrefix)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Network.WebSockets as WS
import Quotes (Quote, QuotesSet)
import Thock (GameState, HasRoomId (..), HasUsername (..), RoomId)

-- | The state of a client in a waiting room
data RoomClientState = RoomClientState
  { _username :: T.Text,
    _isReady :: Bool
  }
  deriving (Generic)

makeFieldsNoPrefix ''RoomClientState

instance FromJSON RoomClientState

instance ToJSON RoomClientState

-- | The state of a client in an online game
data GameClientState = GameClientState
  { _username :: T.Text,
    _progress :: Float,
    _wpm :: Double
  }
  deriving (Generic)

makeFieldsNoPrefix ''GameClientState

instance FromJSON GameClientState

instance ToJSON GameClientState

-- | A waiting room client with a state and connection to the server
data RoomClient = RoomClient
  { _state :: RoomClientState,
    _connection :: WS.Connection
  }

makeFieldsNoPrefix ''RoomClient

-- | An in-game client with a state and connection to the server
data GameClient = GameClient
  { _state :: GameClientState,
    _connection :: WS.Connection
  }

makeFieldsNoPrefix ''GameClient

-- | A message that a client sends to the server to update their state
data ClientMessage
  = RoomClientUpdate RoomClientState
  | GameClientUpdate GameClientState
  | BackToLobby T.Text
  deriving (Generic)

instance FromJSON ClientMessage

instance ToJSON ClientMessage

-- | A message from the server that updates a client on the state of other clients
data ServerMessage
  = RoomUpdate [RoomClientState]
  | GameUpdate [GameClientState]
  | StartGame Quote [GameClientState]
  deriving (Generic)

instance FromJSON ServerMessage

instance ToJSON ServerMessage

-- | Custom event that triggers when the client receives a 'ServerMessage'
newtype ConnectionTick = ConnectionTick ServerMessage

-- | The state of a player waiting to start an online game
data WaitingRoomState = WaitingRoomState
  { -- | Which room the player is in
    _roomId :: RoomId,
    -- | Chosen set of quotes
    _quotesSet :: QuotesSet,
    -- | Information about the player
    _localState :: RoomClientState,
    -- | The player's connection to the server
    _connection :: WS.Connection,
    -- | The states of other players
    _otherPlayers :: [RoomClientState]
  }

makeFieldsNoPrefix ''WaitingRoomState

-- | The state of a player's online game
data OnlineGameState = OnlineGameState
  { -- | The state of the player's personal game
    _localGame :: GameState,
    -- | Room to return to after the game is done
    _roomId :: RoomId,
    -- | Chosen set of quotes
    _quotesSet :: QuotesSet,
    -- | The player's username
    _username :: T.Text,
    -- | The player's connection to the server
    _connection :: WS.Connection,
    -- | The states of other players
    _otherPlayers :: [GameClientState]
  }

makeFieldsNoPrefix ''OnlineGameState

-- | The current status of the online connection
data Online
  = WaitingRoom WaitingRoomState
  | OnlineGame OnlineGameState

-- | Sends the given data over the connection as text using 'encode'
sendJsonData :: ToJSON a => WS.Connection -> a -> IO ()
sendJsonData conn a = WS.sendTextData conn (encode a)

-- | Receives JSON text data from the connection and tries to 'decode' it into the given type.
-- Produces an error if the JSON fails to decode (malformed or wrong type).
receiveJsonData :: FromJSON a => WS.Connection -> IO a
receiveJsonData conn = fromMaybe (error "could not decode JSON message into desired type") . decode <$> WS.receiveData conn
