{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Server where

import Control.Concurrent
  ( MVar,
    modifyMVar,
    modifyMVar_,
    newMVar,
    readMVar,
  )
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.TH
import qualified Network.WebSockets as WS
import Online
import Quotes
import Thock

data ServerState = ServerState {_serverQuote :: Quote, _rooms :: Map RoomId [RoomClient], _activeGames :: Map RoomId [GameClient]} -- TODO: rooms that are started and not yet?
  deriving (Generic)

makeLenses ''ServerState

main :: IO ()
main = runServer

newServerState :: Quote -> ServerState
newServerState q = ServerState {_serverQuote = q, _rooms = Map.empty, _activeGames = Map.empty}

-- numClients :: ServerState -> Int
-- numClients ss = undefined
--   -- length (ss ^. clients) -- TODO: s instead of ss

-- clientExists :: Client -> ServerState -> Bool
-- clientExists client ss = undefined
--   -- any (((/=) `on` (^. (state . clientName))) client) (ss ^. clients)

addClient :: RoomId -> RoomClient -> ServerState -> ServerState
addClient room client ss = ss & rooms %~ Map.adjust (client :) room

removeClient :: RoomId -> RoomClient -> ServerState -> ServerState
removeClient room client ss = ss & rooms %~ Map.adjust (filter (((/=) `on` (^. (roomState . clientUsername))) client)) room

updateClient :: RoomId -> RoomClientState -> ServerState -> ServerState
updateClient room client ss =
  ss & rooms %~ Map.adjust (map (\c -> if c ^. (roomState . clientUsername) == client ^. clientUsername then c & roomState .~ client else c)) room

createRoom :: RoomId -> RoomClient -> ServerState -> ServerState
createRoom room client ss = ss & rooms %~ Map.insert room [client]

runServer :: IO ()
runServer = do
  q <- generateQuote
  st <- newMVar (newServerState q)
  WS.runServer "127.0.0.1" 9160 $ application st

application :: MVar ServerState -> WS.ServerApp
application mState pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    -- TODO: probably want to break into room and game parts
    -- _ <- readMVar mState >>= (\s -> sendJsonData conn (s ^. serverQuote))
    (RoomFormData (Username user) room, isCreating) <- receiveJsonData conn
    let client = RoomClient (RoomClientState user False) conn
        disconnect = do
          -- Remove client and return new state
          s <- modifyMVar mState $ \s ->
            let s' = removeClient room client s in return (s', s')
          roomBroadcast room s
    flip finally disconnect $ do
      if isCreating
        then do
          modifyMVar_ mState $ \s ->
            return $ createRoom room client s
          roomTalk conn mState room
        else do
          ss <- readMVar mState
          if Map.member room (ss ^. rooms)
            then do
              modifyMVar_ mState $ \s -> do
                let s' = addClient room client s
                sendJsonData conn (Just (map (^. roomState) ((s' ^. rooms) Map.! room))) -- TODO: dont want to send both
                roomBroadcast room s'
                return s'
              roomTalk conn mState room
            else sendJsonData conn (Nothing :: Maybe [RoomClientState])

roomTalk :: WS.Connection -> MVar ServerState -> RoomId -> IO ()
roomTalk conn mState room = forever $ do
  c <- receiveJsonData conn
  s <- modifyMVar mState $ \s ->
    let s' = updateClient room c s in return (s', s')
  roomBroadcast room s

-- cs <- receiveJsonData conn
-- -- ss <- readMVar state
-- case cs of
--   _
--     | otherwise -> flip finally disconnect $ do
--       modifyMVar_ mState $ \s -> do
--         let s' = addClient client s
--         broadcast s'
--         return s'
--       talk conn mState
--     where
--       client = Client {_state = cs, _connection = conn}
--       disconnect = do
--         -- Remove client and return new state
--         s <- modifyMVar mState $ \s ->
--           let s' = removeClient client s in return (s', s')
--         broadcast s

-- talk :: WS.Connection -> MVar ServerState -> IO ()
-- talk conn mState = forever $ do
--   c <- receiveJsonData conn
--   s <- modifyMVar mState $ \s ->
--     let s' = updateClient c s in return (s', s')
--   broadcast s

roomBroadcast :: RoomId -> ServerState -> IO ()
roomBroadcast room ss = forM_ cs $ \(RoomClient _ conn) -> sendJsonData conn (map (^. roomState) cs)
  where
    cs = (ss ^. rooms) Map.! room

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
