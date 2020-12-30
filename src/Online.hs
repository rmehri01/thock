{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Online where

import Control.Lens
import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import GHC.Generics
import qualified Network.WebSockets as WS
import Quotes
import Thock

data RoomClientState = RoomClientState
  { _username :: T.Text,
    _isReady :: Bool
  }
  deriving (Generic)

makeFieldsNoPrefix ''RoomClientState

instance FromJSON RoomClientState

instance ToJSON RoomClientState

data GameClientState = GameClientState
  { _username :: T.Text,
    _progress :: Float,
    _wpm :: Double
  }
  deriving (Generic)

makeFieldsNoPrefix ''GameClientState

instance FromJSON GameClientState

instance ToJSON GameClientState

data RoomClient = RoomClient
  { _state :: RoomClientState,
    _connection :: WS.Connection
  }

makeFieldsNoPrefix ''RoomClient

data GameClient = GameClient
  { _state :: GameClientState,
    _connection :: WS.Connection
  }

makeFieldsNoPrefix ''GameClient

data ClientToServerMessage
  = RoomClientUpdate RoomClientState
  | GameClientUpdate GameClientState
  | BackToLobby T.Text
  deriving (Generic)

instance FromJSON ClientToServerMessage

instance ToJSON ClientToServerMessage

data ServerToClientMessage
  = RoomUpdate [RoomClientState]
  | GameUpdate [GameClientState]
  | StartGame Quote [GameClientState]
  deriving (Generic)

instance FromJSON ServerToClientMessage

instance ToJSON ServerToClientMessage

newtype ConnectionTick = ConnectionTick ServerToClientMessage

data Online = Online
  { _localGame :: Game,
    _roomId :: RoomId,
    _username :: T.Text,
    _connection :: WS.Connection,
    _otherPlayers :: [GameClientState]
  }

makeFieldsNoPrefix ''Online

data WaitingRoom = WaitingRoom
  { _roomId :: RoomId,
    _localState :: RoomClientState,
    _connection :: WS.Connection,
    _otherPlayers :: [RoomClientState]
  }

makeFieldsNoPrefix ''WaitingRoom

data OnlineGameState
  = WaitingRoomState WaitingRoom
  | OnlineGame Online

sendJsonData :: ToJSON a => WS.Connection -> a -> IO ()
sendJsonData conn a = WS.sendTextData conn (encode a)

receiveJsonData :: FromJSON a => WS.Connection -> IO a
receiveJsonData conn = fromMaybe (error "could not decode JSON message into desired type") . decode <$> WS.receiveData conn

canStart :: [RoomClientState] -> Bool
canStart rs = length rs > 1 && all (^. isReady) rs
