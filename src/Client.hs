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
import Lens.Micro
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import Online
import Thock
import UI.Online

--------------------------------------------------------------------------------
createApp :: Bool -> RoomFormData -> WS.ClientApp ()
createApp isCreating (RoomFormData (Username user) room) conn = do
  connChan <- newBChan 10

  -- fork a thread that writes custom events when received from server
  _ <-
    forkFinally
      ( forever $ do
          cs <- WS.receiveData conn
          writeBChan connChan (ConnectionTick cs)
      )
      (const $ return ()) -- terminate when connection is closed and ignore any exceptions

  -- TODO: cases for creating and use room to communicate with server
  -- TODO: handle if joining room doesnt exist

  q <- receiveJsonData conn -- TODO: handle possibility of failure
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  let o = initialOnline q user conn
  _ <- sendJsonData conn (ClientState {_clientName = user, _clientProgress = progress (o ^. localGame), _clientWpm = calculateWpm (o ^. localGame)})
  _ <- customMain initialVty buildVty (Just connChan) onlineApp o
  WS.sendClose conn ("Bye!" :: Text)

--------------------------------------------------------------------------------
runClient :: Bool -> RoomFormData -> IO ()
runClient isCreating formData = withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" (createApp isCreating formData)
