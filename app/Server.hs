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
import GHC.Conc (yield)

data ServerState = ServerState {_serverQuote :: Quote, _rooms :: Map RoomId [Client]} -- TODO: rooms that are started and not yet?
  deriving (Generic)

makeLenses ''ServerState

main :: IO ()
main = runServer

newServerState :: Quote -> ServerState
newServerState q = ServerState {_serverQuote = q, _rooms = Map.empty}

-- numClients :: ServerState -> Int
-- numClients ss = undefined
--   -- length (ss ^. clients) -- TODO: s instead of ss

-- clientExists :: Client -> ServerState -> Bool
-- clientExists client ss = undefined
--   -- any (((/=) `on` (^. (state . clientName))) client) (ss ^. clients)

addClient :: RoomId -> Client -> ServerState -> ServerState
addClient room client ss = ss & rooms %~ Map.adjust (client :) room

removeClient :: Client -> RoomId -> ServerState -> ServerState
removeClient client room ss = ss & rooms %~ Map.adjust (filter (((/=) `on` (^. (state . clientName))) client)) room

-- updateClient :: ClientState -> ServerState -> ServerState
-- updateClient cs ss = undefined
--   -- ss & rooms %~ map (\c -> if c ^. (state . clientName) == cs ^. clientName then c & state .~ cs else c)

createRoom :: RoomId -> Client -> ServerState -> ServerState
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
    let client = Client (ClientState user 0 0) conn
        disconnect = do
          -- Remove client and return new state
          s <- modifyMVar mState $ \s ->
            let s' = removeClient client room s in return (s', s')
          broadcast room s
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
                let s' = addClient room client s
                sendJsonData conn (Just (map (^. state) ((s' ^. rooms) Map.! room))) -- TODO: dont want to send both
                broadcast room s'
                return s'
              talk conn mState room
            else sendJsonData conn (Nothing :: Maybe [ClientState])

talk :: WS.Connection -> MVar ServerState -> RoomId -> IO ()
talk _ _ _ = forever $ do
  yield

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

broadcast :: RoomId -> ServerState -> IO ()
broadcast room ss = forM_ cs $ \(Client _ conn) -> sendJsonData conn (map (^. (state . clientName)) cs)
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
