-- | This module allows a client to connect to and interact with the websocket server.
module Client
  ( runClient,
  )
where

import Brick (customMain)
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkFinally)
import Control.Monad (forever)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import Online
  ( ConnectionTick (ConnectionTick),
    Online (WaitingRoom),
    RoomClientState (RoomClientState),
    WaitingRoomState (WaitingRoomState),
    receiveJsonData,
    sendJsonData,
  )
import Thock (RoomFormData (RoomFormData), Username (Username))
import UI.Online (onlineApp)

-- | Sets up the initial state of the 'Online' state using the formData
-- based on if the player is creating the room.
createClientApp :: Bool -> RoomFormData -> WS.ClientApp (Maybe T.Text)
createClientApp isCreating formData@(RoomFormData (Username user) room) conn = do
  _ <- sendJsonData conn (formData, isCreating)

  if isCreating
    then startRoom []
    else do
      res <- receiveJsonData conn
      case res of
        Right others -> startRoom others
        Left msg -> return (Just msg) -- Stop connection due to error message received from server
  where
    localSt = RoomClientState user False
    startRoom others = do
      connChan <- newBChan 10
      -- fork a thread that writes custom events when received from server
      _ <-
        forkFinally
          ( forever $ do
              serverMessage <- receiveJsonData conn
              writeBChan connChan (ConnectionTick serverMessage)
          )
          (const $ return ()) -- terminate when connection is closed and ignore any exceptions
      let buildVty = V.mkVty V.defaultConfig
      initialVty <- buildVty
      let w = WaitingRoom (WaitingRoomState room localSt conn others)
      _ <- customMain initialVty buildVty (Just connChan) onlineApp w
      WS.sendClose conn ("Bye!" :: T.Text)
      return Nothing -- Online connection finished successfully with no errors

-- | Connects to the websocket server and runs the client app
runClient :: Bool -> RoomFormData -> IO (Maybe T.Text)
runClient isCreating formData = withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" (createClientApp isCreating formData)
