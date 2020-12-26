{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Online where

import Control.Concurrent
  ( MVar,
    modifyMVar,
    modifyMVar_,
    newMVar,
    readMVar,
  )
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Function
import Data.Maybe
import qualified Data.Text as T
import GHC.Generics
import Lens.Micro
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

data ServerState = ServerState {_serverQuote :: Quote, _clients :: [Client]}
  deriving (Generic)

makeLenses ''ServerState

newtype ConnectionTick = ConnectionTick ByteString

data Online = Online {_localGame :: Game, _onlineName :: T.Text, _onlineConnection :: WS.Connection, _clientStates :: [ClientState]}

initialOnline :: Quote -> T.Text -> WS.Connection -> Online
initialOnline q name conn = Online {_localGame = initializeGame q, _onlineName = name, _onlineConnection = conn, _clientStates = []}

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

updateClient :: ClientState -> ServerState -> ServerState
updateClient cs ss = ss & clients %~ map (\c -> if c ^. (state . clientName) == cs ^. clientName then c & state .~ cs else c)

runServer :: IO ()
runServer = do
  q <- generateQuote
  st <- newMVar (newServerState q)
  WS.runServer "127.0.0.1" 9160 $ application st

application :: MVar ServerState -> WS.ServerApp
application mState pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    _ <- readMVar mState >>= (\s -> sendJsonData conn (s ^. serverQuote))
    cs <- receiveJsonData conn
    -- ss <- readMVar state
    case cs of
      _
        | otherwise -> flip finally disconnect $ do
          modifyMVar_ mState $ \s -> do
            let s' = addClient client s
            broadcast s'
            return s'
          talk conn mState
        where
          client = Client {_state = cs, _connection = conn}
          disconnect = do
            -- Remove client and return new state
            s <- modifyMVar mState $ \s ->
              let s' = removeClient client s in return (s', s')
            broadcast s

talk :: WS.Connection -> MVar ServerState -> IO ()
talk conn mState = forever $ do
  c <- receiveJsonData conn
  s <- modifyMVar mState $ \s ->
    let s' = updateClient c s in return (s', s')
  broadcast s

broadcast :: ServerState -> IO ()
broadcast ss = forM_ cs $ \(Client _ conn) -> sendJsonData conn (map (^. state) cs)
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
sendJsonData :: ToJSON a => WS.Connection -> a -> IO ()
sendJsonData conn a = WS.sendTextData conn (encode a)

receiveJsonData :: FromJSON a => WS.Connection -> IO a
receiveJsonData conn = fromMaybe (error "could not decode JSON message into desired type") . decode <$> WS.receiveData conn