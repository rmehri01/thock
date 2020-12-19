{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Online where

import           Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar,
                                     readMVar)
import           Control.Exception  (finally)
import           Control.Monad      (forM_, forever)
import           Data.Aeson
import           Data.Function
import           Data.Maybe
import qualified Data.Text          as T
import           GHC.Generics
import           Lens.Micro
import           Lens.Micro.TH
import qualified Network.WebSockets as WS
import           Quotes
import           Thock

-- TODO: working but client and server are out of sync, can we just send whole list, also how tf do you retain the name (might have to save local client state and other client states).
data PlayerStatusMessage = Add | Update | Remove -- TODO: send this on each update? what about people who just joined -> wont get updates until later, could send them state of server on join?

data ClientState = ClientState {_clientName :: T.Text, _clientProgress :: Double, _clientWpm :: Int} -- TODO: overlapping, make better use of lens
  deriving (Generic)

makeLenses ''ClientState

instance FromJSON ClientState where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

instance ToJSON ClientState where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

instance WS.WebSocketsData ClientState where
  fromDataMessage d = case d of
    WS.Text b _ -> fromJust $ decode b -- TODO use mt
    WS.Binary b -> fromJust $ decode b
  fromLazyByteString = fromJust . decode -- TODO: sus
  toLazyByteString = encode

data Client = Client {_state :: ClientState, _connection :: WS.Connection}

makeLenses ''Client

data ServerState = ServerState {_serverQuote :: Quote, _clients :: [Client]}
  deriving (Generic)

makeLenses ''ServerState

newtype ConnectionTick = ConnectionTick WS.Connection

data Online = Online {_localGame :: Game, _clientStates :: [ClientState]}

initialOnline :: Quote -> Online -- TODO: very sus
initialOnline q = Online {_localGame = initializeGame q, _clientStates = []}

makeLenses ''Online

newServerState :: Quote -> ServerState
newServerState q = ServerState {_serverQuote = q, _clients = []}

numClients :: ServerState -> Int
numClients ss = length (ss ^. clients) -- TODO: s instead of ss

clientExists :: Client -> ServerState -> Bool
clientExists client ss = any (((/=) `on` (^. (state . clientName))) client) (ss ^. clients)

addClient :: Client -> ServerState -> ServerState
addClient client ss = ss & clients %~ (:) client

removeClient :: Client -> ServerState -> ServerState
removeClient client ss = ss & clients %~ filter (((/=) `on` (^. (state . clientName))) client)

main :: IO ()
main = do
  q <- generateQuote
  st <- newMVar (newServerState q)
  WS.runServer "127.0.0.1" 9160 $ application st

application :: MVar ServerState -> WS.ServerApp
application mState pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    _ <- readMVar mState >>= (\s -> WS.sendTextData conn (s ^. serverQuote))
    cs <- WS.receiveData conn
    -- ss <- readMVar state
    case cs of
      _
        | otherwise -> flip finally disconnect $ do
          modifyMVar_ mState $ \s -> do
            let s' = addClient client s
            broadcast (client ^. state) s'
            return s'
          talk conn mState
        where
          client = Client {_state = cs, _connection = conn}
          disconnect = do
            -- Remove client and return new state
            s <- modifyMVar mState $ \s ->
              let s' = removeClient client s in return (s', s')
            broadcast (client ^. state) s

talk :: WS.Connection -> MVar ServerState -> IO () -- TODO: client arg?
talk conn mState = forever $ do
  c <- WS.receiveData conn
  readMVar mState >>= broadcast c

broadcast :: ClientState -> ServerState -> IO () -- TODO: need to send all users at once?
broadcast c ss = forM_ cs $ \(Client _ conn) -> WS.sendTextData conn c
  where
    cs = ss ^. clients

-- TODO: first case

-- | not (prefix `T.isPrefixOf` ss) ->
--   WS.sendTextData conn ("Wrong announcement" :: Text)
-- TODO: second case, validation
-- | any
--     ($ fst client)
--     [T.null, T.any isPunctuation, T.any isSpace] ->
--   WS.sendTextData
--     conn
--     ( "Name cannot "
--         <> "contain punctuation or whitespace, and "
--         <> "cannot be empty" :: Text
--     )
-- TODO: third case, already exists validation
-- | clientExists client cs ->
--   WS.sendTextData conn ("User already exists" :: Text)
