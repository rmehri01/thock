{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Lens
import Control.Monad (forM_, forever)
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Network.WebSockets as WS
import Online
import Quotes
import Thock

data ServerState = ServerState
  { _serverQuote :: Quote,
    _rooms :: Map RoomId [RoomClient],
    _activeGames :: Map RoomId [GameClient]
  }
  deriving (Generic)

makeLenses ''ServerState

main :: IO ()
main = runServer

newServerState :: Quote -> ServerState
newServerState q = ServerState {_serverQuote = q, _rooms = Map.empty, _activeGames = Map.empty}

addRoomClient :: RoomId -> RoomClient -> ServerState -> ServerState
addRoomClient room client ss = ss & rooms %~ Map.adjust (client :) room

clientExists :: RoomId -> T.Text -> ServerState -> Bool
clientExists room user ss = any (\r -> r ^. (state . username) == user) ((ss ^. rooms) Map.! room)

removeRoomClient :: RoomId -> T.Text -> ServerState -> ServerState
removeRoomClient room user ss = ss & rooms %~ Map.adjust (deleteFirstEqualUsername user) room

updateRoomClient :: RoomId -> RoomClientState -> ServerState -> ServerState
updateRoomClient room client ss = ss & rooms %~ updateClient room client

updateGameClient :: RoomId -> GameClientState -> ServerState -> ServerState
updateGameClient room client ss = ss & activeGames %~ updateClient room client

updateClient :: (Ord k, HasState b a1, HasUsername a1 a2, Eq a2) => k -> a1 -> Map k [b] -> Map k [b]
updateClient room client = Map.adjust (map updateIfClient) room
  where
    updateIfClient other =
      if other ^. (state . username) == client ^. username
        then other & state .~ client
        else other

createRoom :: RoomId -> RoomClient -> ServerState -> ServerState
createRoom room client ss = ss & rooms %~ Map.insert room [client]

makeActive :: RoomId -> ServerState -> ServerState
makeActive room ss =
  ss & rooms %~ Map.delete room
    & activeGames %~ Map.insert room clients
  where
    clients = map (\(RoomClient (RoomClientState user _) conn) -> GameClient (GameClientState user 0 0) conn) states
    states = (ss ^. rooms) Map.! room

runServer :: IO ()
runServer = do
  q <- generateQuote
  st <- newMVar (newServerState q)
  WS.runServer "127.0.0.1" 9160 $ application st

application :: MVar ServerState -> WS.ServerApp
application mState pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    (RoomFormData (Username user) room, isCreating) <- receiveJsonData conn

    let client = RoomClient (RoomClientState user False) conn
        disconnect = do
          -- TODO: disconnect from room and game separate?
          -- Remove client and return new state
          s <- modifyMVar mState $ \s ->
            let s' = removeRoomClient room user s in return (s', s')
          roomBroadcastExceptSending room user s

    flip finally disconnect $ do
      if isCreating
        then do
          modifyMVar_ mState $ \s ->
            return (createRoom room client s)
          talk conn mState room
        else do
          ss <- readMVar mState
          let response
                | not $ Map.member room (ss ^. rooms) = sendJsonData conn (Left "room does not exist" :: Either T.Text [RoomClientState])
                | clientExists room user ss = sendJsonData conn (Left "username already exists in that room" :: Either T.Text [RoomClientState])
                | otherwise = do
                  modifyMVar_ mState $ \s -> do
                    let s' = addRoomClient room client s
                    let m = (s' ^. rooms) Map.! room
                    let rsExceptUser = deleteFirstEqualUsername user m
                    sendJsonData conn (Right (map (^. state) rsExceptUser) :: Either T.Text [RoomClientState])
                    roomBroadcastExceptSending room user s'
                    return s'
                  talk conn mState room
          response

talk :: WS.Connection -> MVar ServerState -> RoomId -> IO ()
talk conn mState room = forever $ do
  cMessage <- receiveJsonData conn
  case cMessage of
    RoomClientUpdate r -> do
      ss <- modifyMVar mState $ \s ->
        let s' = updateRoomClient room r s in return (s', s')
      if canStart (map (^. state) $ (ss ^. rooms) Map.! room)
        then do
          newS <- modifyMVar mState $ \s -> do
            let s' = makeActive room s in return (s', s')
          broadcastTo id room (StartGame (newS ^. serverQuote)) (newS ^. activeGames)
        else roomBroadcastExceptSending room (r ^. username) ss
    GameClientUpdate g -> do
      s <- modifyMVar mState $ \s ->
        let s' = updateGameClient room g s in return (s', s')
      gameBroadcastExceptSending room (g ^. username) s

gameBroadcastExceptSending :: RoomId -> T.Text -> ServerState -> IO ()
gameBroadcastExceptSending room sending ss = broadcastTo (deleteFirstEqualUsername sending) room GameUpdate (ss ^. activeGames)

roomBroadcastExceptSending :: RoomId -> T.Text -> ServerState -> IO ()
roomBroadcastExceptSending room sending ss = broadcastTo (deleteFirstEqualUsername sending) room RoomUpdate (ss ^. rooms)

broadcastTo ::
  ( Eq a1,
    HasConnection b WS.Connection,
    ToJSON d,
    HasState a c,
    HasState b a4,
    HasUsername c a1,
    HasUsername a4 a1,
    Ord k
  ) =>
  ([a] -> [b]) ->
  k ->
  ([c] -> d) ->
  Map k [a] ->
  IO ()
broadcastTo f room ctr m =
  forM_
    csFiltered
    $ \a -> sendJsonData (a ^. connection) (ctr . map (^. state) $ deleteFirstEqualUsername (a ^. (state . username)) cs)
  where
    csFiltered = f cs
    cs = m Map.! room

deleteFirstEqualUsername :: (Eq a1, HasState s a2, HasUsername a2 a1) => a1 -> [s] -> [s]
deleteFirstEqualUsername user = deleteFirstWhere (\c -> (c ^. (state . username)) == user)

deleteFirstWhere :: (a -> Bool) -> [a] -> [a]
deleteFirstWhere _ [] = []
deleteFirstWhere p (a : as) = if p a then as else a : deleteFirstWhere p as
