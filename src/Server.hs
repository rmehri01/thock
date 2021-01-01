{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module deals with maintaining the state of the server when receiving updates.
module Server where

import Control.Concurrent
  ( MVar,
    modifyMVar,
    modifyMVar_,
    newMVar,
    readMVar,
  )
import Control.Exception (finally)
import Control.Lens (makeLenses, (%~), (&), (.~), (^.))
import Control.Monad (forM_, forever)
import Data.Aeson (ToJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Network.WebSockets as WS
import Online
  ( ClientMessage
      ( BackToLobby,
        GameClientUpdate,
        RoomClientUpdate
      ),
    GameClient (GameClient),
    GameClientState (GameClientState),
    HasConnection (..),
    HasIsReady (isReady),
    HasState (..),
    RoomClient (RoomClient),
    RoomClientState (RoomClientState),
    ServerMessage (GameUpdate, RoomUpdate, StartGame),
    receiveJsonData,
    sendJsonData,
  )
import Quotes (generateQuote)
import Thock
  ( HasUsername (..),
    RoomFormData (RoomFormData),
    RoomId,
    Username (Username),
  )

data ServerState = ServerState
  { _rooms :: Map RoomId [RoomClient],
    _activeGames :: Map RoomId [GameClient]
  }
  deriving (Generic)

makeLenses ''ServerState

-- | Runs the websocket server with a random quote and initial state
runServer :: Int -> IO ()
runServer port = do
  st <- newMVar newServerState
  WS.runServer "0.0.0.0" port $ serverApp st

-- | Handles interacting with incoming client connections
serverApp :: MVar ServerState -> WS.ServerApp
serverApp mState pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    (RoomFormData (Username user) room, isCreating) <- receiveJsonData conn

    let client = RoomClient (RoomClientState user False) conn
        disconnect = do
          s <- modifyMVar mState $ \s ->
            let s' = removeRoomClient room user s
             in return (s', s')
          roomBroadcastExceptSending room user s

    if isCreating
      then flip finally disconnect $ do
        modifyMVar_ mState $ \s ->
          return (createRoom room client s)
        talk conn mState room
      else do
        ss <- readMVar mState
        let response
              | not $ Map.member room (ss ^. rooms) = sendJsonData conn (Left "room does not exist" :: Either T.Text [RoomClientState])
              | clientExists room user ss = sendJsonData conn (Left "username already exists in that room" :: Either T.Text [RoomClientState])
              | otherwise = flip finally disconnect $ do
                modifyMVar_ mState $ \s -> do
                  let s' = addRoomClient room client s
                  let m = (s' ^. rooms) Map.! room
                  let rsExceptUser = deleteFirstEqualUsername user m
                  sendJsonData conn (Right (map (^. state) rsExceptUser) :: Either T.Text [RoomClientState])
                  roomBroadcastExceptSending room user s'
                  return s'
                talk conn mState room
        response

-- | Communicates with a single client by receiving and sending updates through conn
talk :: WS.Connection -> MVar ServerState -> RoomId -> IO ()
talk conn mState room = forever $ do
  cMessage <- receiveJsonData conn
  case cMessage of
    RoomClientUpdate r -> do
      ss <- modifyMVar mState $ \s ->
        let s' = updateRoomClient room r s in return (s', s')
      -- if can start and game not already in progress, send start game, else just update
      if canStart (map (^. state) $ (ss ^. rooms) Map.! room)
        && isNothing (Map.lookup room (ss ^. activeGames))
        then do
          newS <- modifyMVar mState $ \s -> do
            let s' = makeActive room s in return (s', s')
          q <- generateQuote
          broadcastTo id room (StartGame q) (newS ^. activeGames)
        else roomBroadcastExceptSending room (r ^. username) ss
    GameClientUpdate g -> do
      s <- modifyMVar mState $ \s ->
        let s' = updateGameClient room g s in return (s', s')
      gameBroadcastExceptSending room (g ^. username) s
    BackToLobby user -> do
      s <- modifyMVar mState $ \s ->
        let client = RoomClient (RoomClientState user False) conn
            leftGame = removeGameClient room user s
            s' = addRoomClient room client leftGame
         in return (s', s')
      let m = (s ^. rooms) Map.! room
      let rsExceptUser = deleteFirstEqualUsername user m
      sendJsonData conn (RoomUpdate $ map (^. state) rsExceptUser)
      gameBroadcastExceptSending room user s
      roomBroadcastExceptSending room user s

-- | Creates the initial 'ServerState' with empty rooms and games
newServerState :: ServerState
newServerState = ServerState {_rooms = Map.empty, _activeGames = Map.empty}

-- | Adds a 'RoomClient' into the given waiting room
addRoomClient :: RoomId -> RoomClient -> ServerState -> ServerState
addRoomClient room client ss = ss & rooms %~ Map.adjust (client :) room

-- | Produces true if the given user is in either the waiting room or an active game
clientExists :: RoomId -> T.Text -> ServerState -> Bool
clientExists room user ss = case Map.lookup room (ss ^. activeGames) of
  Nothing -> inRoom
  Just cs -> inRoom || any (equalOnStateUsername user) cs
  where
    inRoom = any (equalOnStateUsername user) ((ss ^. rooms) Map.! room)

-- | Removes a user from the given waiting room.
-- Deletes the room if it is empty and there is no active game for that room.
removeRoomClient :: RoomId -> T.Text -> ServerState -> ServerState
removeRoomClient room user ss =
  case Map.lookup room (ss ^. activeGames) of
    Nothing -> ss & rooms %~ removeClient room user
    _ -> ss & rooms %~ Map.adjust (deleteFirstEqualUsername user) room

-- | Removes a user from the active game and deletes it if empty
removeGameClient :: RoomId -> T.Text -> ServerState -> ServerState
removeGameClient room user ss = ss & activeGames %~ removeClient room user

-- | Remove a user from the room in m and delete the room if it is empty
removeClient :: (Ord k, HasState s a2, HasUsername a2 a1, Eq a1) => k -> a1 -> Map k [s] -> Map k [s]
removeClient room user m =
  if null (withoutUser Map.! room)
    then Map.delete room withoutUser
    else withoutUser
  where
    withoutUser = Map.adjust (deleteFirstEqualUsername user) room m

-- | Updates the state of a 'RoomClient' based on username in the given waiting room
updateRoomClient :: RoomId -> RoomClientState -> ServerState -> ServerState
updateRoomClient room client ss = ss & rooms %~ updateClient room client

-- | Updates the state of a 'GameClient' based on username in the given active game room
updateGameClient :: RoomId -> GameClientState -> ServerState -> ServerState
updateGameClient room client ss = ss & activeGames %~ updateClient room client

-- | Updates the state of a client based on username in the given room in a Map
updateClient :: (Ord k, HasState b a1, HasUsername a1 a2, Eq a2) => k -> a1 -> Map k [b] -> Map k [b]
updateClient room client = Map.adjust (map updateIfClient) room
  where
    updateIfClient other =
      if equalOnStateUsername (client ^. username) other
        then other & state .~ client
        else other

-- | Creates a new waiting room with the given client in it
createRoom :: RoomId -> RoomClient -> ServerState -> ServerState
createRoom room client ss = ss & rooms %~ Map.insert room [client]

-- | Moves all the players in a waiting room to an active game, leaving the waiting room empty.
makeActive :: RoomId -> ServerState -> ServerState
makeActive room ss =
  ss & rooms %~ Map.insert room []
    & activeGames %~ Map.insert room clients
  where
    clients = map (\(RoomClient (RoomClientState user _) conn) -> GameClient (GameClientState user 0 0) conn) states
    states = (ss ^. rooms) Map.! room

-- | Sends an update of the 'ServerState' to everyone in a waiting room
-- except the one who triggered the update
roomBroadcastExceptSending :: RoomId -> T.Text -> ServerState -> IO ()
roomBroadcastExceptSending room sending ss =
  broadcastTo (deleteFirstEqualUsername sending) room RoomUpdate (ss ^. rooms)

-- | Sends an update of the 'ServerState' to everyone in an active game
-- except the one who triggered the update
gameBroadcastExceptSending :: RoomId -> T.Text -> ServerState -> IO ()
gameBroadcastExceptSending room sending ss =
  broadcastTo (deleteFirstEqualUsername sending) room GameUpdate (ss ^. activeGames)

-- | Sends an update of the 'ServerState' to everyone in a room
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
  -- | Function to filter who to send the updates to
  ([a] -> [b]) ->
  -- | Room to send to
  k ->
  -- | Constructor for creating the desired type to send
  ([c] -> d) ->
  -- | All of the rooms
  Map k [a] ->
  IO ()
broadcastTo f room ctr m =
  forM_
    csFiltered
    $ \a ->
      sendJsonData
        (a ^. connection)
        (ctr . map (^. state) $ deleteFirstEqualUsername (a ^. (state . username)) cs)
  where
    csFiltered = f cs
    cs = fromMaybe [] (Map.lookup room m)

-- | Removes the first in the given list where the inner state's username is equal to user
deleteFirstEqualUsername :: (Eq a1, HasState s a2, HasUsername a2 a1) => a1 -> [s] -> [s]
deleteFirstEqualUsername user = deleteFirstWhere (equalOnStateUsername user)

-- | Removes the first value from the list where the predicate p is satisfied
deleteFirstWhere :: (a -> Bool) -> [a] -> [a]
deleteFirstWhere _ [] = []
deleteFirstWhere p (a : as) = if p a then as else a : deleteFirstWhere p as

-- | Produces true if the inner state's username is equal to the given user
equalOnStateUsername :: (Eq a1, HasState s a2, HasUsername a2 a1) => a1 -> s -> Bool
equalOnStateUsername user c = (c ^. (state . username)) == user

-- | Produces true if there is more than one person in the room and they are all ready
canStart :: [RoomClientState] -> Bool
canStart rs = length rs > 1 && all (^. isReady) rs