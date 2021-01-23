-- | This module provides the UI for a game once an online connection is established.
module UI.Online where

import Brick
  ( BrickEvent (AppEvent, VtyEvent),
    EventM,
    Next,
    Widget,
    showFirstCursor,
    txt,
    txtWrap,
    vBox,
    withAttr,
    (<+>),
    (<=>),
  )
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import Control.Lens ((%~), (&), (+~), (.~), (^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Graphics.Vty as V
import Online
  ( ClientMessage (BackToLobby, GameClientUpdate, RoomClientUpdate),
    ConnectionTick (..),
    GameClientState (GameClientState),
    HasConnection (connection),
    HasIsReady (isReady),
    HasLocalGame (localGame),
    HasOtherPlayers (otherPlayers),
    HasProgress (progress),
    HasWpm (wpm),
    Online (..),
    OnlineGameState (OnlineGameState),
    RoomClientState (RoomClientState),
    ServerMessage (GameUpdate, RoomUpdate, StartGame),
    WaitingRoomState (WaitingRoomState),
    sendJsonData,
  )
import Thock
  ( HasRoomId (roomId),
    HasUsername (username),
    ResourceName,
    calculateProgress,
    calculateWpm,
    initializeGameState,
    isDone,
    strokes,
  )
import UI.Attributes (attributeMap, redAttr, secondaryAttr)
import UI.Common
  ( addBorder,
    drawFinished,
    drawInput,
    drawProgressBar,
    drawProgressBarGameState,
    drawPrompt,
    updateGameState,
  )

-- | Brick app for handling an 'Online' game
onlineApp :: M.App Online ConnectionTick ResourceName
onlineApp =
  M.App
    { M.appDraw = drawOnline,
      M.appChooseCursor = showFirstCursor,
      M.appHandleEvent = handleKeyOnline,
      M.appStartEvent = return,
      M.appAttrMap = const attributeMap
    }

-- | Draws given 'Online' based on current state
drawOnline :: Online -> [Widget ResourceName]
drawOnline s = case s of
  WaitingRoom w -> drawWaitingRoom w
  OnlineGame o -> drawOnlineState o

-- | Creates a waiting room widget which displays the 'RoomId',
-- all connected players and their status, as well as a help section
drawWaitingRoom :: WaitingRoomState -> [Widget ResourceName]
drawWaitingRoom (WaitingRoomState room localSt _ ps) =
  [roomIdWidget <=> (playersDisplay <+> statusDisplay) <=> helpWidget]
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
        then withAttr secondaryAttr (txt "ready")
        else withAttr redAttr (txt "not ready")
    allStates = localSt : ps

-- | Creates a display with the local game state and the progress of other players
drawOnlineState :: OnlineGameState -> [Widget ResourceName]
drawOnlineState o =
  [ drawFinished g "Back to lobby: Esc",
    drawProgressBarGameState g <=> otherProgressBars <=> drawPrompt g <=> drawInput g
  ]
  where
    otherProgressBars =
      vBox $
        map
          (\gs -> drawProgressBar (gs ^. progress) (gs ^. wpm) (gs ^. username))
          (o ^. otherPlayers)
    g = o ^. localGame

-- | Handles an event based on current 'Online' state
handleKeyOnline :: Online -> BrickEvent ResourceName ConnectionTick -> EventM ResourceName (Next Online)
handleKeyOnline s ev = case s of
  WaitingRoom w -> handleKeyWaitingRoom w ev
  OnlineGame o -> handleKeyOnlineState o ev

-- | Handles both local and 'ConnectionTick' events when the player is in a 'WaitingRoom'
handleKeyWaitingRoom :: WaitingRoomState -> BrickEvent ResourceName ConnectionTick -> EventM ResourceName (Next Online)
handleKeyWaitingRoom (WaitingRoomState room localSt conn _) (AppEvent (ConnectionTick csReceived)) =
  case csReceived of
    RoomUpdate rs -> M.continue (WaitingRoom $ WaitingRoomState room localSt conn rs)
    StartGame q gs -> M.continue (OnlineGame (OnlineGameState (initializeGameState q) room (localSt ^. username) conn gs))
    _ -> undefined
handleKeyWaitingRoom (WaitingRoomState room localSt conn ps) (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt (WaitingRoom $ WaitingRoomState room localSt conn ps)
    V.EvKey (V.KChar 'q') [] -> M.halt $ WaitingRoom $ WaitingRoomState room localSt conn ps
    V.EvKey (V.KChar 'r') [] -> do
      let newSt = localSt & isReady %~ not
      liftIO (sendJsonData conn (RoomClientUpdate newSt))
      M.continue (WaitingRoom $ WaitingRoomState room newSt conn ps)
    _ -> M.continue (WaitingRoom $ WaitingRoomState room localSt conn ps)
handleKeyWaitingRoom (WaitingRoomState room localSt conn ps) _ = M.continue (WaitingRoom $ WaitingRoomState room localSt conn ps)

-- | Handles both local and 'ConnectionTick' events when the player is in an 'OnlineGame'
handleKeyOnlineState :: OnlineGameState -> BrickEvent ResourceName ConnectionTick -> EventM ResourceName (Next Online)
handleKeyOnlineState o (AppEvent (ConnectionTick csReceived)) =
  case csReceived of
    RoomUpdate rs -> M.continue (WaitingRoom $ WaitingRoomState (o ^. roomId) (RoomClientState (o ^. username) False) (o ^. connection) rs)
    GameUpdate gs -> M.continue (OnlineGame $ o & otherPlayers .~ gs)
    _ -> undefined
handleKeyOnlineState o (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> exit
    V.EvKey (V.KChar 'q') [V.MCtrl] -> exit
    V.EvKey (V.KChar _) [] -> nextState $ numStrokes o
    _ -> nextState o
  where
    exit = liftIO (sendJsonData (o ^. connection) (BackToLobby $ o ^. username)) >> M.continue (OnlineGame o)
    numStrokes o' =
      if isDone (o' ^. localGame)
      then o'
      else o' & (localGame . strokes) +~ 1

    nextState o' =
      if isDone (o' ^. localGame)
        then M.continue (OnlineGame o')
        else do
          updatedGame <- updateGameState (o' ^. localGame) ev
          let newClientState = GameClientState (o ^. username) (calculateProgress updatedGame) (calculateWpm updatedGame)
          _ <-
            liftIO $
              sendJsonData
                (o ^. connection)
                (GameClientUpdate newClientState)
          M.continue (OnlineGame $ o' & localGame .~ updatedGame)
handleKeyOnlineState o _ = M.continue (OnlineGame o)
