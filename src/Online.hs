{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Online where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Maybe
import qualified Data.Text as T
import GHC.Generics
import Lens.Micro.TH
import qualified Network.WebSockets as WS
import Quotes
import Thock

data ClientState = ClientState {_clientName :: T.Text, _clientProgress :: Float, _clientWpm :: Double} -- TODO: overlapping, make better use of lens
  deriving (Generic)

makeLenses ''ClientState

instance FromJSON ClientState where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

instance ToJSON ClientState where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

data Client = Client {_state :: ClientState, _connection :: WS.Connection}

makeLenses ''Client

newtype ConnectionTick = ConnectionTick ByteString

data Online = Online {_localGame :: Game, _onlineName :: T.Text, _onlineConnection :: WS.Connection, _clientStates :: [ClientState]}

makeLenses ''Online

data OnlineGameState
  = WaitingRoom {_roomId :: RoomId, _players :: [T.Text]}
  | OnlineGame Online

makeLenses ''OnlineGameState

initialOnline :: Quote -> T.Text -> WS.Connection -> Online
initialOnline q name conn = Online {_localGame = initializeGame q, _onlineName = name, _onlineConnection = conn, _clientStates = []}

sendJsonData :: ToJSON a => WS.Connection -> a -> IO ()
sendJsonData conn a = WS.sendTextData conn (encode a)

receiveJsonData :: FromJSON a => WS.Connection -> IO a
receiveJsonData conn = fromMaybe (error "could not decode JSON message into desired type") . decode <$> WS.receiveData conn