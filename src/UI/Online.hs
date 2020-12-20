module UI.Online where

import Brick
import qualified Brick.Main as M
import Control.Monad.IO.Class
import Data.Foldable
import qualified Graphics.Vty as V
import Lens.Micro
import qualified Network.WebSockets as WS
import Online
import Thock
import UI.Attributes
import UI.Common

onlineApp :: M.App Online ConnectionTick ()
onlineApp =
  M.App
    { M.appDraw = drawOnline,
      M.appChooseCursor = showFirstCursor,
      M.appHandleEvent = handleKeyOnline,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }

drawOnline :: Online -> [Widget ()]
drawOnline o = [drawFinished g, drawProgressBarGame g <=> foldl' (\w c -> w <=> drawProgressBar (c ^. clientProgress) (c ^. clientWpm)) emptyWidget (o ^. clientStates) <=> drawPrompt g <=> drawInput g]
  where
    g = o ^. localGame

handleKeyOnline :: Online -> BrickEvent () ConnectionTick -> EventM () (Next Online)
handleKeyOnline o (AppEvent (ConnectionTick conn)) = do
  Update csReceived <- liftIO $ WS.receiveData conn -- TODO: handle possibility of failure
  _ <- liftIO $ WS.sendTextData conn (ClientState {_clientName = o ^. onlineName, _clientProgress = progress (o ^. localGame), _clientWpm = calculateWpm (o ^. localGame)}) -- TODO: not sure about sending here
  M.continue (o & clientStates .~ filter (\cs -> cs ^. clientName /= o ^. onlineName) csReceived)
handleKeyOnline o (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt o
    V.EvKey (V.KChar _) [] -> nextState (o & (localGame . strokes) +~ 1)
    _ -> nextState o
  where
    nextState o' =
      if isDone (o' ^. localGame)
        then M.continue o'
        else updateGame (o' ^. localGame) ev >>= M.continue . (\g -> o' & localGame .~ g)
handleKeyOnline o _ = M.continue o
