module UI.Online where

import Brick
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import Control.Monad.IO.Class
import Data.Foldable
import qualified Graphics.Vty as V
import Lens.Micro
import qualified Network.WebSockets as WS
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
  WaitingRoom room localSt _ ps -> drawWaitingRoom room localSt ps
  OnlineGame o -> drawOnline o

drawWaitingRoom :: RoomId -> RoomClientState -> [RoomClientState] -> [Widget ResourceName]
drawWaitingRoom room localSt ps = [roomIdWidget <=> (playersDisplay <+> statusDisplay) <=> helpWidget]
  where
    roomIdWidget = C.center $ addBorder "room id" (txt room)
    playersDisplay = C.center $ addBorder "players" (foldr ((\user w -> txt user <=> w) . (^. clientUsername)) emptyWidget (localSt : ps))
    statusDisplay =
      -- TODO: extract
      C.center $
        addBorder
          "statuses"
          ( foldr
              ( ( \ready w ->
                    (if ready then withAttr correctAttr (txt "yes") else withAttr incorrectAttr (txt "no")) <=> w
                )
                  . (^. isReady)
              )
              emptyWidget
              (localSt : ps)
          )
    helpWidget = addBorder "help" (txt "Press 'r' to ready up! Once everyone is ready, the match will begin.")

drawOnline :: Online -> [Widget ResourceName]
drawOnline o = [drawFinished g, drawProgressBarGame g <=> foldl' (\w c -> w <=> drawProgressBar (c ^. clientProgress) (c ^. clientWpm)) emptyWidget (o ^. clientStates) <=> drawPrompt g <=> drawInput g]
  where
    g = o ^. localGame

handleKeyOnlineState :: OnlineGameState -> BrickEvent ResourceName ConnectionTick -> EventM ResourceName (Next OnlineGameState)
handleKeyOnlineState s ev = case s of
  WaitingRoom room localSt conn ps -> handleKeyWaitingRoom room localSt conn ps ev
  OnlineGame o -> handleKeyOnline o ev

-- TODO: pass all arguments in together or separate?
handleKeyWaitingRoom :: RoomId -> RoomClientState -> WS.Connection -> [RoomClientState] -> BrickEvent ResourceName ConnectionTick -> EventM ResourceName (Next OnlineGameState)
handleKeyWaitingRoom room localSt conn _ (AppEvent (ConnectionTick csReceived)) =
  case csReceived of
    RoomUpdate rs -> M.continue (WaitingRoom room localSt conn rs)
    StartGame q gs -> M.continue (OnlineGame (Online {_localGame = initializeGame q, _onlineName = localSt ^. clientUsername, _onlineConnection = conn, _clientStates = gs})) -- TODO: use initial online
    _ -> error "undefined behaviour"
handleKeyWaitingRoom room localSt conn ps (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt (WaitingRoom room localSt conn ps)
    V.EvKey (V.KChar 'r') [] -> do
      let newSt = localSt & isReady %~ not
      liftIO (sendJsonData conn (RoomClientUpdate newSt))
      M.continue (WaitingRoom room newSt conn ps)
    _ -> M.continue (WaitingRoom room localSt conn ps)
handleKeyWaitingRoom room localSt conn ps _ = M.continue (WaitingRoom room localSt conn ps)

handleKeyOnline :: Online -> BrickEvent ResourceName ConnectionTick -> EventM ResourceName (Next OnlineGameState)
handleKeyOnline o (AppEvent (ConnectionTick (GameUpdate csReceived))) = do
  M.continue (OnlineGame $ o & clientStates .~ filter (\cs -> cs ^. clientName /= o ^. onlineName) csReceived)
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
          _ <- liftIO $ sendJsonData (o ^. onlineConnection) (GameClientUpdate $ GameClientState {_clientName = o ^. onlineName, _clientProgress = progress updatedGame, _clientWpm = calculateWpm updatedGame})
          M.continue (OnlineGame $ o' & localGame .~ updatedGame)
handleKeyOnline o _ = M.continue (OnlineGame o)
