module UI.Online where -- TODO: name might be confused

import           Brick
import qualified Brick.Main             as M
import qualified Brick.Widgets.Edit     as E
import           Control.Monad.IO.Class
import           Data.Foldable
import qualified Data.Function
import           Data.Time
import qualified Graphics.Vty           as V
import           Lens.Micro
import qualified Network.WebSockets     as WS
import           Online
import           Thock
import           UI.Attributes
import           UI.Common

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
drawOnline o = [drawFinished g, drawProgressBar g <=> foldl' (\w c -> w <=> txt (c ^. clientName)) emptyWidget (o ^. clientStates) <=> drawPrompt g <=> drawInput g]
  where
    g = o ^. localGame

handleKeyOnline :: Online -> BrickEvent () ConnectionTick -> EventM () (Next Online)
handleKeyOnline o (AppEvent (ConnectionTick conn)) = do
  cReceived <- liftIO $ WS.receiveData conn -- TODO: handle possibility of failure
  _ <- liftIO $ WS.sendTextData conn (ClientState {_clientName = "frink", _clientProgress = realToFrac $ progress (o ^. localGame), _clientWpm = floor $ wpm (o ^. localGame)})
  M.continue (o & clientStates %~ (\cs -> cReceived : filter (((/=) `Data.Function.on` (^. clientName)) cReceived) cs))
handleKeyOnline o (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc []      -> M.halt o
    -- V.EvKey (V.KChar 'b') [V.MCtrl] -> M.continue initialState
    -- V.EvKey (V.KChar 'r') [V.MCtrl] -> startGameM (Just $ g ^. quote) (Practice g)
    -- V.EvKey (V.KChar 'n') [V.MCtrl] -> startGameM Nothing (Practice g)
    V.EvKey (V.KChar _) [] -> nextState (o & (localGame . strokes) +~ 1)
    _                      -> nextState o
  where
    nextState :: Online -> EventM () (Next Online)
    nextState o' =
      if isDone (o' ^. localGame)
        then M.continue o'
        else do
          gEdited <- handleEventLensed (o' ^. localGame) input E.handleEditorEvent ev
          currentTime <- liftIO getCurrentTime
          M.continue (o' & localGame .~ updateTime currentTime (movePromptCursor gEdited))
handleKeyOnline o _ = M.continue o
