module UI.Online where

import Brick
import qualified Brick.Main as M
import Control.Monad.IO.Class
import Data.Aeson
import Data.Foldable
import Data.Maybe
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro
import Online
import Thock
import UI.Attributes
import UI.Common
import qualified Brick.Widgets.Center as C

onlineApp :: M.App OnlineGameState ConnectionTick ResourceName
onlineApp =
  M.App
    { M.appDraw = drawOnlineState,
      M.appChooseCursor = showFirstCursor,
      M.appHandleEvent = handleKeyOnlineState,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }

drawOnlineState :: OnlineGameState -> [Widget ResourceName]
drawOnlineState s = case s of
  WaitingRoom room ps -> drawWaitingRoom room ps
  OnlineGame o -> drawOnline o

drawWaitingRoom :: RoomId -> [T.Text] -> [Widget ResourceName]
drawWaitingRoom room ps = [roomIdWidget <=> playersDisplay]
  where
    roomIdWidget = C.center $ addBorder "room id" (txt room)
    playersDisplay = C.center $ addBorder "players" (foldr (\user w -> txt user <=> w) emptyWidget ps)

drawOnline :: Online -> [Widget ResourceName]
drawOnline o = [drawFinished g, drawProgressBarGame g <=> foldl' (\w c -> w <=> drawProgressBar (c ^. clientProgress) (c ^. clientWpm)) emptyWidget (o ^. clientStates) <=> drawPrompt g <=> drawInput g]
  where
    g = o ^. localGame

handleKeyOnlineState :: OnlineGameState -> BrickEvent ResourceName ConnectionTick -> EventM ResourceName (Next OnlineGameState)
handleKeyOnlineState s ev = case s of
  WaitingRoom room ps -> handleKeyWaitingRoom room ps ev
  OnlineGame o -> handleKeyOnline o ev

handleKeyWaitingRoom :: RoomId -> [T.Text] -> BrickEvent ResourceName ConnectionTick -> EventM ResourceName (Next OnlineGameState)
handleKeyWaitingRoom room _ (AppEvent (ConnectionTick csReceived)) = 
  M.continue (WaitingRoom room (fromJust $ decode csReceived))
handleKeyWaitingRoom room ps (VtyEvent _) | _ <- V.EvKey V.KEsc [] = M.halt (WaitingRoom room ps)
handleKeyWaitingRoom room ps _ = M.continue (WaitingRoom room ps)

handleKeyOnline :: Online -> BrickEvent ResourceName ConnectionTick -> EventM ResourceName (Next OnlineGameState)
handleKeyOnline o (AppEvent (ConnectionTick csReceived)) = do
  M.continue (OnlineGame $ o & clientStates .~ filter (\cs -> cs ^. clientName /= o ^. onlineName) (fromJust $ decode csReceived))
handleKeyOnline o (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt (OnlineGame o)
    V.EvKey (V.KChar _) [] -> nextState (o & (localGame . strokes) +~ 1)
    _ -> nextState o
  where
    nextState o' =
      if isDone (o' ^. localGame)
        then M.continue (OnlineGame o')
        else do
          updatedGame <- updateGame (o' ^. localGame) ev
          _ <- liftIO $ sendJsonData (o ^. onlineConnection) (ClientState {_clientName = o ^. onlineName, _clientProgress = progress updatedGame, _clientWpm = calculateWpm updatedGame})
          M.continue (OnlineGame $ o' & localGame .~ updatedGame)
handleKeyOnline o _ = M.continue (OnlineGame o)
