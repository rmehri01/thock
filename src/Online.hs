{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Online where

import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import GHC.Generics
import Lens.Micro
import Lens.Micro.TH
import qualified Network.WebSockets as WS
import Quotes
import Thock

-- TODO: different module?

data RoomClientState = RoomClientState {_clientUsername :: T.Text, _isReady :: Bool}
  deriving (Generic)

makeLenses ''RoomClientState

instance FromJSON RoomClientState where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1} -- TODO: field modifiers might not be needed

instance ToJSON RoomClientState where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

data GameClientState = GameClientState {_clientName :: T.Text, _clientProgress :: Float, _clientWpm :: Double} -- TODO: overlapping, make better use of lens
  deriving (Generic)

makeLenses ''GameClientState

instance FromJSON GameClientState where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

instance ToJSON GameClientState where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

data RoomClient = RoomClient {_roomState :: RoomClientState, _roomConnection :: WS.Connection}

makeLenses ''RoomClient

data GameClient = GameClient {_state :: GameClientState, _connection :: WS.Connection}

makeLenses ''GameClient

data ClientToServerMessage
  = RoomClientUpdate RoomClientState
  | GameClientUpdate GameClientState
  deriving (Generic)

instance FromJSON ClientToServerMessage

instance ToJSON ClientToServerMessage

data ServerToClientMessage
  = RoomUpdate [RoomClientState]
  | -- | JoinRoomConfirmation (Maybe [RoomClientState])
    GameUpdate [GameClientState]
  | StartGame Quote [GameClientState]
  deriving (Generic)

instance FromJSON ServerToClientMessage

instance ToJSON ServerToClientMessage

newtype ConnectionTick = ConnectionTick ServerToClientMessage

data Online = Online {_localGame :: Game, _onlineName :: T.Text, _onlineConnection :: WS.Connection, _clientStates :: [GameClientState]}

makeLenses ''Online

data OnlineGameState
  = WaitingRoom {_roomId :: RoomId, _localState :: RoomClientState, _waitingRoomConnection :: WS.Connection, _otherPlayers :: [RoomClientState]}
  | OnlineGame Online

makeLenses ''OnlineGameState

initialOnline :: Quote -> T.Text -> WS.Connection -> Online
initialOnline q name conn = Online {_localGame = initializeGame q, _onlineName = name, _onlineConnection = conn, _clientStates = []}

sendJsonData :: ToJSON a => WS.Connection -> a -> IO ()
sendJsonData conn a = WS.sendTextData conn (encode a)

receiveJsonData :: FromJSON a => WS.Connection -> IO a
receiveJsonData conn = fromMaybe (error "could not decode JSON message into desired type") . decode <$> WS.receiveData conn

canStart :: [RoomClientState] -> Bool
canStart rs = length rs > 1 && all (^. isReady) rs
