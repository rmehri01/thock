module UI.Offline where

import Brick
import Brick.Forms
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import Client
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro
import Quotes
import Thock
import UI.Attributes
import UI.Common

draw :: GameState -> [Widget ResourceName]
draw s = case s of
  MainMenu l -> drawList l
  OnlineSelect l -> drawList l
  CreateRoomMenu form -> drawForm form
  JoinRoomMenu form -> drawForm form
  Practice g -> drawPractice g

drawList :: MenuList -> [Widget ResourceName]
drawList l = [addBorder "" (titleWidget <=> listWidget)]
  where
    listWidget = vLimitPercent 20 $ L.renderList listDrawElement True l

drawForm :: RoomForm a -> [Widget ResourceName]
drawForm form = [C.center (renderForm form)]

handleKey :: GameState -> BrickEvent ResourceName () -> EventM ResourceName (Next GameState)
handleKey gs ev = case gs of
  MainMenu l -> handleKeyMainMenu l ev
  OnlineSelect l -> handleKeyOnlineSelect l ev
  CreateRoomMenu form -> handleKeyForm CreateRoomMenu (\u -> generateRoomId >>= runClient True . RoomFormData u) form ev
  JoinRoomMenu form -> handleKeyForm JoinRoomMenu (runClient False) form ev
  Practice g -> handleKeyPractice g ev

handleKeyMainMenu :: MenuList -> BrickEvent ResourceName e -> EventM ResourceName (Next GameState)
handleKeyMainMenu l (VtyEvent e) = case e of
  V.EvKey V.KEsc [] -> M.halt (MainMenu l)
  V.EvKey V.KEnter []
    | Just i <- L.listSelected l ->
      if i == 0
        then liftIO generateQuote >>= M.continue . startPracticeGame
        else M.continue onlineSelectState
  ev -> L.handleListEvent ev l >>= M.continue . MainMenu
handleKeyMainMenu l _ = M.continue (MainMenu l)

handleKeyOnlineSelect :: MenuList -> BrickEvent ResourceName e -> EventM ResourceName (Next GameState)
handleKeyOnlineSelect l (VtyEvent e) = case e of
  V.EvKey V.KEsc [] -> M.continue initialState
  V.EvKey V.KEnter []
    | Just i <- L.listSelected l ->
      M.continue
        ( if i == 0
            then CreateRoomMenu (mkCreateRoomForm emptyUsername)
            else JoinRoomMenu (mkJoinRoomForm emptyRoomFormData)
        )
    where
      emptyUsername = Username ""
      emptyRoomFormData = RoomFormData emptyUsername ""
  ev -> L.handleListEvent ev l >>= M.continue . OnlineSelect
handleKeyOnlineSelect l _ = M.continue (OnlineSelect l)

handleKeyForm ::
  (RoomForm a -> GameState) ->
  (a -> IO ()) ->
  RoomForm a ->
  BrickEvent ResourceName () ->
  EventM ResourceName (Next GameState)
handleKeyForm ctr onEnter form ev@(VtyEvent e) = case e of
  V.EvKey V.KEsc [] -> M.continue initialState
  V.EvKey V.KEnter [] -> M.suspendAndResume (onEnter (formState form) >> return initialState)
  _ -> handleFormEvent ev form >>= M.continue . ctr
handleKeyForm ctr _ form _ = M.continue (ctr form)

mkCreateRoomForm :: Username -> RoomForm Username
mkCreateRoomForm =
  newForm
    [ formLabel "Username"
        @@= editTextField value UsernameField (Just 1)
    ]

mkJoinRoomForm :: RoomFormData -> RoomForm RoomFormData
mkJoinRoomForm =
  newForm
    [ formLabel "Username"
        @@= editTextField (username . value) UsernameField (Just 1),
      formLabel "Room ID"
        @@= editTextField roomId RoomIdField (Just 1)
    ]

formLabel :: T.Text -> Widget n -> Widget n
formLabel t w =
  padBottom (Pad 1) $
    vLimit 1 (hLimit 15 $ txt t <+> fill ' ') <+> w

handleKeyPractice :: Game -> BrickEvent ResourceName e -> EventM ResourceName (Next GameState)
handleKeyPractice g (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt (Practice g)
    V.EvKey (V.KChar 'b') [V.MCtrl] -> M.continue initialState
    V.EvKey (V.KChar 'r') [V.MCtrl] -> M.continue (startPracticeGame (g ^. quote))
    V.EvKey (V.KChar 'n') [V.MCtrl] -> liftIO generateQuote >>= M.continue . startPracticeGame
    V.EvKey (V.KChar _) [] -> nextState (g & strokes +~ 1)
    _ -> nextState g
  where
    nextState g' =
      if isDone g'
        then M.continue (Practice g')
        else updateGame g' ev >>= M.continue . Practice
handleKeyPractice g _ = M.continue (Practice g)

theApp :: M.App GameState () ResourceName
theApp =
  M.App
    { M.appDraw = draw,
      M.appChooseCursor = showFirstCursor,
      M.appHandleEvent = handleKey,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }
