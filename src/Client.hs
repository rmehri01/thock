--------------------------------------------------------------------------------
module Client
  ( runClient,
  )
where

--------------------------------------------------------------------------------
import Brick
import Brick.BChan
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.Text (Text)
import qualified Graphics.Vty as V
import Lens.Micro
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import Online
import Quotes
import Thock
import UI.Online

--------------------------------------------------------------------------------
app :: WS.ClientApp ()
app conn = do
  connChan <- newBChan 10

  -- Fork a thread that writes the connection to the custom event
  _ <- forkIO $
    forever $ do
      writeBChan connChan (ConnectionTick conn) -- TODO: non blocking?
      threadDelay 500000 -- tick every 0.5 seconds
  q <- WS.receiveData conn -- TODO: handle possibility of failure
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  name <- generateQuote -- TODO: get name from user
  let o = initialOnline q (name ^. source)
  _ <- WS.sendTextData conn (ClientState {_clientName = name ^. source, _clientProgress = progress (o ^. localGame), _clientWpm = calculateWpm (o ^. localGame)})
  _ <- customMain initialVty buildVty (Just connChan) onlineApp o
  WS.sendClose conn ("Bye!" :: Text)

--------------------------------------------------------------------------------
runClient :: IO ()
runClient = withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" app
