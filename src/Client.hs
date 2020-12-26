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
createApp isCreating formData@(RoomFormData (Username user) room) conn = do
  connChan <- newBChan 10
  -- let startOnlineGame name = do
  --       q <- receiveJsonData conn -- TODO: handle possibility of failure
  --       let buildVty = V.mkVty V.defaultConfig
  --       initialVty <- buildVty
  --       let o = initialOnline q name conn
  --       _ <- sendJsonData conn (ClientState {_clientName = name, _clientProgress = progress (o ^. localGame), _clientWpm = calculateWpm (o ^. localGame)})
  --       _ <- customMain initialVty buildVty (Just connChan) onlineApp o
  --       WS.sendClose conn ("Bye!" :: Text)
  let startRoom ps = do
        -- fork a thread that writes custom events when received from server
        _ <-
          forkFinally
            ( forever $ do
                cs <- WS.receiveData conn
                writeBChan connChan (ConnectionTick cs)
            )
            (const $ return ()) -- terminate when connection is closed and ignore any exceptions
        let buildVty = V.mkVty V.defaultConfig
        initialVty <- buildVty
        let o = WaitingRoom room ps
        _ <- customMain initialVty buildVty (Just connChan) onlineApp o
        WS.sendClose conn ("Bye!" :: Text)

  _ <- sendJsonData conn (formData, isCreating)

  if isCreating
    then startRoom [user]
    else do
      res <- receiveJsonData conn
      case res of
        Just ps -> startRoom (map (^. clientName) ps)
        Nothing -> error "room doesnt exist" -- TODO: more friendly exit

--------------------------------------------------------------------------------
runClient :: Bool -> RoomFormData -> IO ()
runClient isCreating formData = withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" (createApp isCreating formData)
