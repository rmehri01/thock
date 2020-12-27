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
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.TH
import qualified Network.WebSockets as WS
import Online
import Quotes
import Thock

data ServerState = ServerState {_serverQuote :: Quote, _rooms :: Map RoomId [RoomClient], _activeGames :: Map RoomId [GameClient]}
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

addRoomClient :: RoomId -> RoomClient -> ServerState -> ServerState
addRoomClient room client ss = ss & rooms %~ Map.adjust (client :) room

removeRoomClient :: RoomId -> RoomClient -> ServerState -> ServerState
removeRoomClient room client ss = ss & rooms %~ Map.adjust (deleteFirstEqualOn (^. (roomState . clientUsername)) client) room

updateRoomClient :: RoomId -> RoomClientState -> ServerState -> ServerState
updateRoomClient room client ss = ss & rooms %~ Map.adjust (map updateIfClient) room
  where
    updateIfClient other =
      if other ^. (roomState . clientUsername) == client ^. clientUsername
        then other & roomState .~ client
        else other

updateGameClient :: RoomId -> GameClientState -> ServerState -> ServerState
updateGameClient room client ss = ss & activeGames %~ Map.adjust (map updateIfClient) room
  where
    updateIfClient other =
      if other ^. (state . clientName) == client ^. clientName
        then other & state .~ client
        else other

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
          -- TODO: disconnect from room and game separate?
          -- Remove client and return new state
          s <- modifyMVar mState $ \s ->
            let s' = removeRoomClient room client s in return (s', s')
          roomBroadcastExceptSending room user s
    flip finally disconnect $ do
      if isCreating
        then do
          modifyMVar_ mState $ \s ->
            return $ createRoom room client s
          talk conn mState room
        else do
          ss <- readMVar mState
          if Map.member room (ss ^. rooms)
            then do
              modifyMVar_ mState $ \s -> do
                let s' = addRoomClient room client s
                sendJsonData conn (Just (clientStatesExceptSelf (client ^. roomState) ((s' ^. rooms) Map.! room)))
                roomBroadcastExceptSending room user s'
                return s'
              talk conn mState room
            else sendJsonData conn (Nothing :: Maybe [RoomClientState])

talk :: WS.Connection -> MVar ServerState -> RoomId -> IO ()
talk conn mState room = forever $ do
  cMessage <- receiveJsonData conn
  case cMessage of
    RoomClientUpdate r -> do
      -- TODO: abstract
      s <- modifyMVar mState $ \s ->
        let s' = updateRoomClient room r s in return (s', s')
      if canStart (map (^. roomState) $ (s ^. rooms) Map.! room)
        then do
          newS <- modifyMVar mState $ \s' -> do
            let s'' = makeActive room s' in return (s'', s'')
          forM_
            ((newS ^. activeGames) Map.! room)
            $ \(GameClient gs conn') -> sendJsonData conn' (StartGame (newS ^. serverQuote) (deleteFirstEqualOn (^. clientName) gs $ map (^. state) ((newS ^. activeGames) Map.! room))) -- TODO: abstract broadcast
        else do
          roomBroadcastExceptSending room (r ^. clientUsername) s
    GameClientUpdate g -> do
      s <- modifyMVar mState $ \s ->
        let s' = updateGameClient room g s in return (s', s')
      gameBroadcastExceptSending room (g ^. clientName) s

gameBroadcastExceptSending :: RoomId -> T.Text -> ServerState -> IO () -- TODO: abstract
gameBroadcastExceptSending room sending ss =
  forM_
    csFiltered
    $ \(GameClient gs conn) -> sendJsonData conn (GameUpdate $ deleteFirstEqualOn (^. clientName) gs $ map (^. state) cs) -- TODO: generalize
  where
    csFiltered = deleteFirstWhere (\c -> (c ^. (state . clientName)) == sending) cs
    cs = (ss ^. activeGames) Map.! room

makeActive :: RoomId -> ServerState -> ServerState
makeActive room ss =
  ss & rooms %~ Map.delete room
    & activeGames %~ Map.insert room clients
  where
    clients = map (\(RoomClient (RoomClientState user _) conn) -> GameClient (GameClientState user 0 0) conn) states
    states = (ss ^. rooms) Map.! room

-- cs <- receiveJsonData conn
-- -- ss <- readMVar state
-- case cs of
--   _
--     | otherwise -> flip finally disconnect $ do
--       modifyMVar_ mState $ \s -> do
--         let s' = addRoomClient client s
--         broadcast s'
--         return s'
--       talk conn mState
--     where
--       client = Client {_state = cs, _connection = conn}
--       disconnect = do
--         -- Remove client and return new state
--         s <- modifyMVar mState $ \s ->
--           let s' = removeRoomClient client s in return (s', s')
--         broadcast s

-- talk :: WS.Connection -> MVar ServerState -> IO ()
-- talk conn mState = forever $ do
--   c <- receiveJsonData conn
--   s <- modifyMVar mState $ \s ->
--     let s' = updateRoomClient c s in return (s', s')
--   broadcast s

roomBroadcastExceptSending :: RoomId -> T.Text -> ServerState -> IO ()
roomBroadcastExceptSending room sending ss =
  forM_
    csFiltered
    $ \(RoomClient rs conn) -> sendJsonData conn (RoomUpdate $ clientStatesExceptSelf rs cs)
  where
    csFiltered = deleteFirstWhere (\c -> (c ^. (roomState . clientUsername)) == sending) cs
    cs = (ss ^. rooms) Map.! room

clientStatesExceptSelf :: RoomClientState -> [RoomClient] -> [RoomClientState]
clientStatesExceptSelf self = deleteFirstEqualOn (^. clientUsername) self . map (^. roomState)

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
deleteFirstEqualOn :: Eq b => (a -> b) -> a -> [a] -> [a]
deleteFirstEqualOn f toDelete = deleteFirstWhere (((==) `on` f) toDelete)

deleteFirstWhere :: (a -> Bool) -> [a] -> [a]
deleteFirstWhere _ [] = []
deleteFirstWhere p (a : as) = if p a then as else a : deleteFirstWhere p as
