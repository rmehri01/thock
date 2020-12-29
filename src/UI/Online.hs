module UI.Online where

import Brick
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import Control.Lens
import Control.Monad.IO.Class
import qualified Graphics.Vty as V
import Online
import Thock
import UI.Attributes
import UI.Common

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
  WaitingRoomState w -> drawWaitingRoom w
  OnlineGame o -> drawOnline o

drawWaitingRoom :: WaitingRoom -> [Widget ResourceName]
drawWaitingRoom (WaitingRoom room localSt _ ps) = [roomIdWidget <=> (playersDisplay <+> statusDisplay) <=> helpWidget]
  where
    roomIdWidget = addBorder "room id" $ C.hCenter (txt room)
    playersDisplay =
      addBorder "players" . C.center . vBox $
        map (txt . (^. username)) allStates
    statusDisplay =
      addBorder "status" . C.center . vBox $
        map (makeReadyTxt . (^. isReady)) allStates
    helpWidget = addBorder "help" $ C.hCenter (txtWrap "Press 'r' to ready up! Once everyone is ready, the match will begin.")
    makeReadyTxt ready =
      if ready
        then withAttr primaryAttr (txt "ready")
        else withAttr secondaryAttr (txt "not ready")
    allStates = localSt : ps

drawOnline :: Online -> [Widget ResourceName]
drawOnline o = [drawFinished g, drawProgressBarGame g <=> otherProgressBars <=> drawPrompt g <=> drawInput g]
  where
    otherProgressBars =
      vBox $
        map
          (\gs -> drawProgressBar (gs ^. progress) (gs ^. wpm) (gs ^. username))
          (o ^. otherPlayers)
    g = o ^. localGame

handleKeyOnlineState :: OnlineGameState -> BrickEvent ResourceName ConnectionTick -> EventM ResourceName (Next OnlineGameState)
handleKeyOnlineState s ev = case s of
  WaitingRoomState w -> handleKeyWaitingRoom w ev
  OnlineGame o -> handleKeyOnline o ev

handleKeyWaitingRoom :: WaitingRoom -> BrickEvent ResourceName ConnectionTick -> EventM ResourceName (Next OnlineGameState)
handleKeyWaitingRoom (WaitingRoom room localSt conn _) (AppEvent (ConnectionTick csReceived)) =
  case csReceived of
    RoomUpdate rs -> M.continue (WaitingRoomState $ WaitingRoom room localSt conn rs)
    StartGame q gs -> M.continue (OnlineGame (Online (initializeGame q) (localSt ^. username) conn gs))
    _ -> error "undefined behaviour"
handleKeyWaitingRoom (WaitingRoom room localSt conn ps) (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt (WaitingRoomState $ WaitingRoom room localSt conn ps)
    V.EvKey (V.KChar 'r') [] -> do
      let newSt = localSt & isReady %~ not
      liftIO (sendJsonData conn (RoomClientUpdate newSt))
      M.continue (WaitingRoomState $ WaitingRoom room newSt conn ps)
    _ -> M.continue (WaitingRoomState $ WaitingRoom room localSt conn ps)
handleKeyWaitingRoom (WaitingRoom room localSt conn ps) _ = M.continue (WaitingRoomState $ WaitingRoom room localSt conn ps)

handleKeyOnline :: Online -> BrickEvent ResourceName ConnectionTick -> EventM ResourceName (Next OnlineGameState)
handleKeyOnline o (AppEvent (ConnectionTick (GameUpdate csReceived))) = do
  M.continue (OnlineGame $ o & otherPlayers .~ filter (\cs -> cs ^. username /= o ^. username) csReceived)
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
          let newClientState = GameClientState (o ^. username) (calculateProgress updatedGame) (calculateWpm updatedGame)
          _ <-
            liftIO $
              sendJsonData
                (o ^. connection)
                (GameClientUpdate newClientState)
          M.continue (OnlineGame $ o' & localGame .~ updatedGame)
handleKeyOnline o _ = M.continue (OnlineGame o)
