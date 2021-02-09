-- | This module has deals with UI specific to the offline game and for transitioning to online.
module UI.Offline where

import Brick
  ( BrickEvent (VtyEvent),
    EventM,
    Next,
    Padding (Max),
    Widget,
    fill,
    hLimit,
    hLimitPercent,
    padBottom,
    showFirstCursor,
    txt,
    txtWrap,
    vLimit,
    withAttr,
    (<+>),
    (<=>),
  )
import Brick.Forms
  ( Form (formState),
    editTextField,
    handleFormEvent,
    newForm,
    renderForm,
    (@@=),
  )
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import Client (runClient)
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Quotes (QuotesSet (..), generateQuote)
import Thock
  ( Game (..),
    GameState,
    HasRoomId (roomId),
    HasUsername (username),
    MenuList,
    ResourceName (RoomIdField, UsernameField),
    RoomForm,
    RoomFormData (RoomFormData),
    RoomInitData (..),
    Username (Username),
    generateRoomId,
    initialGame,
    isDone,
    onlineSelectLang,
    onlineSelectState,
    pracSelectLang,
    quote,
    quotesSet,
    startPracticeGame,
    strokes,
    uname,
    value,
  )
import UI.Attributes (attributeMap, redAttr, secondaryAttr)
import UI.Common
  ( addBorder,
    drawFinished,
    drawInput,
    drawProgressBarGameState,
    drawPrompt,
    listDrawElement,
    titleWidget,
    updateGameState,
  )

-- | Brick app for handling an offline 'Game'
localApp :: M.App Game () ResourceName
localApp =
  M.App
    { M.appDraw = drawGame,
      M.appChooseCursor = showFirstCursor,
      M.appHandleEvent = handleKeyGame,
      M.appStartEvent = return,
      M.appAttrMap = const attributeMap
    }

-- | Draws the given 'Game' based on its current state
drawGame :: Game -> [Widget ResourceName]
drawGame g = case g of
  MainMenu l -> drawList l
  PracticeSelectLang l -> drawList l
  OnlineSelect l -> drawList l
  OnlineSelectLang l -> drawList l
  CreateRoomMenu form -> drawForm form
  JoinRoomMenu form -> drawForm form
  Practice gs -> drawPractice gs
  ErrorOverlay prev t -> drawError prev t

-- | Renders a list widget under the logo
drawList :: MenuList -> [Widget ResourceName]
drawList l = [drawMenu listWidget]
  where
    listWidget = L.renderList listDrawElement True l

-- | Renders a form widget under the logo
drawForm :: RoomForm a -> [Widget ResourceName]
drawForm form = [drawMenu formWidget]
  where
    formWidget = vLimit 6 $ hLimitPercent 80 (renderForm form)

-- | Draws the title over another widget w
drawMenu :: Widget ResourceName -> Widget ResourceName
drawMenu w = addBorder "" (C.center titleWidget <=> padBottom Max (C.hCenter w))

-- | Draw a practice game based on its 'GameState'
drawPractice :: GameState -> [Widget ResourceName]
drawPractice g =
  [ drawFinished g "Back: Esc | Retry: ^r | Next: Enter / ^n",
    drawProgressBarGameState g <=> drawPrompt g <=> drawInput g
  ]

-- | Draws an error popup over the 'Game'
drawError :: Game -> T.Text -> [Widget ResourceName]
drawError g t = errorPopup : drawGame g
  where
    errorPopup = C.centerLayer . hLimitPercent 80 . addBorder "error" $ withAttr redAttr (txtWrap t)

-- | Handles events based on the current state of the 'Game'
handleKeyGame :: Game -> BrickEvent ResourceName () -> EventM ResourceName (Next Game)
handleKeyGame gs ev = case gs of
  MainMenu l -> handleKeyMainMenu l ev
  PracticeSelectLang l -> handleKeyPracLang l ev
  OnlineSelect l -> handleKeyOnlineSelect l ev
  OnlineSelectLang l -> handleKeyOnlineLang l ev
  CreateRoomMenu form -> handleKeyForm CreateRoomMenu cr cl form ev
  JoinRoomMenu form -> handleKeyForm JoinRoomMenu jr jl form ev
  Practice g -> handleKeyPractice g ev
  ErrorOverlay prev _ -> M.continue prev -- after receiving any event, remove the error overlay
  where
    cr (RoomInitData u qs) = runClient True (Just qs) . RoomFormData u =<< generateRoomId
    jr = runClient False Nothing
    cl = (^. (uname . value))
    jl = (^. (username . value))

-- | Handles key events for navigating the 'MainMenu'
handleKeyMainMenu :: MenuList -> BrickEvent ResourceName e -> EventM ResourceName (Next Game)
handleKeyMainMenu l (VtyEvent e) = case e of
  V.EvKey V.KEsc [] -> exit
  V.EvKey V.KEnter []
    | Just i <- L.listSelected l -> case i of
      0 -> M.continue pracSelectLang
      _ -> M.continue onlineSelectState
  ev -> M.continue . MainMenu =<< L.handleListEvent ev l
  where
    exit = M.halt $ MainMenu l
handleKeyMainMenu l _ = M.continue $ MainMenu l

-- | Handles key events for navigating the 'LangMenu' after choosing practice
handleKeyPracLang :: MenuList -> BrickEvent ResourceName e -> EventM ResourceName (Next Game)
handleKeyPracLang l (VtyEvent e) = case e of
  V.EvKey V.KEsc [] -> M.continue initialGame
  V.EvKey V.KEnter []
    | Just i <- L.listSelected l -> ng $ case i of
      0 -> English
      1 -> Russian
      _ -> Haskell
  ev -> M.continue . PracticeSelectLang =<< L.handleListEvent ev l
  where
    ng qs = M.continue . startPracticeGame qs =<< liftIO (generateQuote qs)

-- | Handles key events for navigating the 'LangMenu' after choosing online
handleKeyOnlineLang :: MenuList -> BrickEvent ResourceName e -> EventM ResourceName (Next Game)
handleKeyOnlineLang l (VtyEvent e) = case e of
  V.EvKey V.KEsc [] -> M.continue onlineSelectState
  V.EvKey V.KEnter []
    | Just i <- L.listSelected l -> ng $ case i of
      0 -> English
      1 -> Russian
      _ -> Haskell
  ev -> M.continue . OnlineSelectLang =<< L.handleListEvent ev l
  where
    ng qs =
      M.continue $
        CreateRoomMenu $
          makeCreateRoomForm $
            RoomInitData (Username "") qs

-- | Handles key events for creating or joining an online room
handleKeyOnlineSelect :: MenuList -> BrickEvent ResourceName e -> EventM ResourceName (Next Game)
handleKeyOnlineSelect l (VtyEvent e) = case e of
  V.EvKey V.KEsc [] -> M.continue initialGame
  V.EvKey V.KEnter []
    | Just i <- L.listSelected l -> M.continue $ case i of
      0 -> onlineSelectLang
      _ -> JoinRoomMenu $ makeJoinRoomForm emptyRoomFormData
    where
      emptyUsername = Username ""
      emptyRoomFormData = RoomFormData emptyUsername ""
  ev -> M.continue . OnlineSelect =<< L.handleListEvent ev l
handleKeyOnlineSelect l _ = M.continue (OnlineSelect l)

-- | Handles a key event for a form and its validation
handleKeyForm ::
  -- | Function to construct a Game from the given form
  (RoomForm a -> Game) ->
  -- | Action to run when the form is submitted, returns a possible error
  (a -> IO (Maybe T.Text)) ->
  -- | Function to get the username from the given form
  (a -> T.Text) ->
  RoomForm a ->
  BrickEvent ResourceName () ->
  EventM ResourceName (Next Game)
handleKeyForm ctr onEnter getUser form ev@(VtyEvent e) = case e of
  V.EvKey V.KEsc [] -> M.continue initialGame
  V.EvKey V.KEnter [] ->
    if not . T.null $ getUser (formState form)
      then
        M.suspendAndResume
          (maybe initialGame (ErrorOverlay (ctr form)) <$> onEnter (formState form))
      else M.continue (ErrorOverlay (ctr form) "username cannot be empty")
  _ -> handleFormEvent ev form >>= M.continue . ctr
handleKeyForm ctr _ _ form _ = M.continue (ctr form)

-- | Construct a 'RoomForm' with the given 'Username' as the initial state
makeCreateRoomForm :: RoomInitData -> RoomForm RoomInitData
makeCreateRoomForm =
  newForm
    [ formLabel "Username"
        @@= addBorder ""
        @@= editTextField (uname . value) UsernameField (Just 1)
    ]

-- | Construct a 'RoomForm' with the given 'RoomFormData' as the initial state
makeJoinRoomForm :: RoomFormData -> RoomForm RoomFormData
makeJoinRoomForm =
  newForm
    [ formLabel "Username"
        @@= addBorder ""
        @@= editTextField (username . value) UsernameField (Just 1),
      formLabel "Room ID"
        @@= addBorder ""
        @@= editTextField roomId RoomIdField (Just 1)
    ]

-- | Label a widget using the given text in the style of a form
formLabel :: T.Text -> Widget n -> Widget n
formLabel t w = C.vCenter label <+> C.vCenter w
  where
    label = vLimit 1 (hLimit 15 $ withAttr secondaryAttr $ txt t <+> fill ' ')

-- | Handles key events for updating the 'GameState' in a 'Practice' game
handleKeyPractice :: GameState -> BrickEvent ResourceName e -> EventM ResourceName (Next Game)
handleKeyPractice g (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.continue initialGame
    V.EvKey (V.KChar 'r') [V.MCtrl] -> rnext
    V.EvKey (V.KChar 'к') [V.MCtrl] -> rnext
    V.EvKey (V.KChar 'n') [V.MCtrl] -> nnext
    V.EvKey (V.KChar 'т') [V.MCtrl] -> nnext
    V.EvKey V.KEnter [] -> nnext
    _ ->
      if isDone g
        then M.continue . Practice $ g
        else M.continue . Practice =<< updateGameState g ev
  where
    q = g ^. quote
    qs = g ^. quotesSet
    rnext = M.continue . startPracticeGame qs $ q
    nnext = M.continue . startPracticeGame qs =<< liftIO (generateQuote qs)
handleKeyPractice g _ = M.continue $ Practice g
