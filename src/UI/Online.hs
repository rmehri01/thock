module UI.Online where

import Brick
import qualified Brick.Main as M
import Control.Monad.IO.Class
import Data.Foldable
import qualified Graphics.Vty as V
import Lens.Micro
import Online
import Thock
import UI.Attributes
import UI.Common
import Data.Aeson
import Data.Maybe

onlineApp :: M.App Online ConnectionTick ResourceName
onlineApp =
  M.App
    { M.appDraw = drawOnline,
      M.appChooseCursor = showFirstCursor,
      M.appHandleEvent = handleKeyOnline,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }

drawOnline :: Online -> [Widget ResourceName]
drawOnline o = [drawFinished g, drawProgressBarGame g <=> foldl' (\w c -> w <=> drawProgressBar (c ^. clientProgress) (c ^. clientWpm)) emptyWidget (o ^. clientStates) <=> drawPrompt g <=> drawInput g]
  where
    g = o ^. localGame

handleKeyOnline :: Online -> BrickEvent ResourceName ConnectionTick -> EventM ResourceName (Next Online)
handleKeyOnline o (AppEvent (ConnectionTick csReceived)) = do
  M.continue (o & clientStates .~ filter (\cs -> cs ^. clientName /= o ^. onlineName) (fromJust $ decode csReceived))
handleKeyOnline o (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt o
    V.EvKey (V.KChar _) [] -> nextState (o & (localGame . strokes) +~ 1)
    _ -> nextState o
  where
    nextState o' =
      if isDone (o' ^. localGame)
        then M.continue o'
        else do
          updatedGame <- updateGame (o' ^. localGame) ev
          _ <- liftIO $ sendJsonData (o ^. onlineConnection) (ClientState {_clientName = o ^. onlineName, _clientProgress = progress updatedGame, _clientWpm = calculateWpm updatedGame})
          M.continue (o' & localGame .~ updatedGame)
handleKeyOnline o _ = M.continue o
