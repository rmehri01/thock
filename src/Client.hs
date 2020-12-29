--------------------------------------------------------------------------------
module Client
  ( runClient,
  )
where

--------------------------------------------------------------------------------
import Brick
import Brick.BChan
import Control.Concurrent (forkFinally)
import Control.Monad (forever)
import Data.Text (Text)
import qualified Graphics.Vty as V
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import Online
import Thock
import UI.Online

--------------------------------------------------------------------------------
createApp :: Bool -> RoomFormData -> WS.ClientApp ()
createApp isCreating formData@(RoomFormData (Username user) room) conn = do
  _ <- sendJsonData conn (formData, isCreating)

  if isCreating
    then startRoom []
    else do
      res <- receiveJsonData conn
      case res of
        Just others -> startRoom others
        Nothing -> error "room doesnt exist" -- TODO: more friendly exit
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
      let o = WaitingRoom room localSt conn others
      _ <- customMain initialVty buildVty (Just connChan) onlineApp o
      WS.sendClose conn ("Bye!" :: Text)

--------------------------------------------------------------------------------
runClient :: Bool -> RoomFormData -> IO ()
runClient isCreating formData = withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" (createApp isCreating formData)
