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
import Data.Bifunctor (second)
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
import Quotes (QuotesSet (..), generateQuote)
import Thock
  ( HasUsername (..),
    RoomFormData (RoomFormData),
    RoomId,
    Username (Username),
  )

data ServerState = ServerState
  { _rooms :: Map RoomId (QuotesSet, [RoomClient]),
    _activeGames :: Map RoomId (QuotesSet, [GameClient])
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
    (RoomFormData (Username user) rId, isCreating, mqs) <- getData conn

    let client = mkClient user conn
        onClose = roomBroadcastExceptSending rId user =<< modifyMVar mState rmAll
          where
            rm st = removeRoomClient rId user st
            rmAll st = return (rm st, rm st)
        qs = fromMaybe English mqs

    if isCreating
      then mkRoom conn onClose rId qs client
      else do
        ss <- readMVar mState
        let response
              | hasRoom rId ss = sendData conn $ Left "room does not exist"
              | clientExists rId user ss =
                sendData conn $ Left "username already exists in that room"
              | otherwise = joinClient conn onClose rId client user
        response
  where
    mkClient u c = RoomClient (RoomClientState u False) c
    getData c = receiveJsonData c :: IO (RoomFormData, Bool, Maybe QuotesSet)
    sendData c d = sendJsonData c (d :: Either T.Text [RoomClientState])
    hasRoom r s = not $ Map.member r (s ^. rooms)
    mkRoom c oc r qs cl = flip finally oc $ do
      modifyMVar_ mState $ \s -> return $ createRoom r qs cl s
      talk c mState r
    joinClient c oc r cl u = flip finally oc $ do
      modifyMVar_ mState $ \s -> do
        let s' = addRoomClient r cl s
        let m = (s' ^. rooms) Map.! r
        let rsExceptUser = deleteFirstEqualUsername u $ snd m
        sendData c $ Right $ map (^. state) rsExceptUser
        roomBroadcastExceptSending r u s'
        return s'
      talk c mState r

-- | Communicates with a single client by receiving and sending updates through conn
talk :: WS.Connection -> MVar ServerState -> RoomId -> IO ()
talk conn mState room = forever $ do
  cMessage <- receiveJsonData conn

  case cMessage of
    RoomClientUpdate r -> do
      ss <- modifyMVar mState $ \s ->
        let s' = updateRoomClient room r s in return (s', s')
      -- if can start and game not already in progress, send start game, else just update
      if canStart (map (^. state) $ snd $ (ss ^. rooms) Map.! room)
        && isNothing (Map.lookup room (ss ^. activeGames))
        then do
          newS <- modifyMVar mState $ \s ->
            let s' = makeActive room s in return (s', s')
          q <- generateQuote $ fst $ (ss ^. rooms) Map.! room
          broadcastTo id room (StartGame q) $ snd <$> (newS ^. activeGames)
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
      let m = snd $ (s ^. rooms) Map.! room
      let rsExceptUser = deleteFirstEqualUsername user m
      sendJsonData conn (RoomUpdate $ map (^. state) rsExceptUser)
      gameBroadcastExceptSending room user s
      roomBroadcastExceptSending room user s

-- | Creates the initial 'ServerState' with empty rooms and games
newServerState :: ServerState
newServerState = ServerState {_rooms = Map.empty, _activeGames = Map.empty}

-- | Adds a 'RoomClient' into the given waiting room
addRoomClient :: RoomId -> RoomClient -> ServerState -> ServerState
addRoomClient room client ss = ss & rooms %~ Map.adjust (second (client :)) room

-- | Produces true if the given user is in either the waiting room or an active game
clientExists :: RoomId -> T.Text -> ServerState -> Bool
clientExists room user ss = case Map.lookup room (ss ^. activeGames) of
  Nothing -> inRoom
  Just cs -> inRoom || any (equalOnStateUsername user) (snd cs)
  where
    inRoom = any (equalOnStateUsername user) (snd $ (ss ^. rooms) Map.! room)

-- | Removes a user from the given waiting room.
-- Deletes the room if it is empty and there is no active game for that room.
removeRoomClient :: RoomId -> T.Text -> ServerState -> ServerState
removeRoomClient room user ss =
  case Map.lookup room (ss ^. activeGames) of
    Nothing -> ss & rooms %~ removeClient room user
    _ -> ss & rooms %~ Map.adjust (second (deleteFirstEqualUsername user)) room

-- | Removes a user from the active game and deletes it if empty
removeGameClient :: RoomId -> T.Text -> ServerState -> ServerState
removeGameClient room user ss = ss & activeGames %~ removeClient room user

-- | Remove a user from the room in m and delete the room if it is empty
removeClient ::
  (Ord k, HasState s a2, HasUsername a2 a1, Eq a1) =>
  k ->
  a1 ->
  Map k (q, [s]) ->
  Map k (q, [s])
removeClient room user m =
  if null $ snd $ withoutUser Map.! room
    then Map.delete room withoutUser
    else withoutUser
  where
    withoutUser = Map.adjust (second (deleteFirstEqualUsername user)) room m

-- | Updates the state of a 'RoomClient' based on username in the given waiting room
updateRoomClient :: RoomId -> RoomClientState -> ServerState -> ServerState
updateRoomClient room client ss = ss & rooms %~ updateClient room client

-- | Updates the state of a 'GameClient' based on username in the given active game room
updateGameClient :: RoomId -> GameClientState -> ServerState -> ServerState
updateGameClient room client ss = ss & activeGames %~ updateClient room client

-- | Updates the state of a client based on username in the given room in a Map
updateClient ::
  (Ord k, HasState b a1, HasUsername a1 a2, Eq a2) =>
  k ->
  a1 ->
  Map k (q, [b]) ->
  Map k (q, [b])
updateClient room client = Map.adjust (second (map updateIfClient)) room
  where
    updateIfClient other =
      if equalOnStateUsername (client ^. username) other
        then other & state .~ client
        else other

-- | Creates a new waiting room with the given client in it
createRoom :: RoomId -> QuotesSet -> RoomClient -> ServerState -> ServerState
createRoom room qs client ss = ss & rooms %~ Map.insert room (qs, [client])

-- | Moves all the players in a waiting room to an active game, leaving the waiting room empty.
makeActive :: RoomId -> ServerState -> ServerState
makeActive room ss =
  ss & rooms %~ Map.insert room (fst states, []) & activeGames %~ Map.insert room clients
  where
    clients = flip second states $
      map $
        \(RoomClient (RoomClientState user _) conn) ->
          GameClient (GameClientState user 0 0) conn
    states = (ss ^. rooms) Map.! room

-- | Sends an update of the 'ServerState' to everyone in a waiting room
-- except the one who triggered the update
roomBroadcastExceptSending :: RoomId -> T.Text -> ServerState -> IO ()
roomBroadcastExceptSending room sending ss =
  broadcastTo (deleteFirstEqualUsername sending) room RoomUpdate $
    snd <$> (ss ^. rooms)

-- | Sends an update of the 'ServerState' to everyone in an active game
-- except the `sending` one who triggered the update
gameBroadcastExceptSending :: RoomId -> T.Text -> ServerState -> IO ()
gameBroadcastExceptSending room sending ss =
  broadcastTo (deleteFirstEqualUsername sending) room GameUpdate $
    snd <$> (ss ^. activeGames)

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
