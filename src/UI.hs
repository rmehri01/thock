{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Edit
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.ProgressBar as P
import qualified Data.Function as F
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro
import Thock

draw :: Game -> [Widget Name]
draw g = [progressWidget <=> promptWidget <=> inputWidget]
  where
    progressWidget = drawProgressBar g
    promptWidget = addBorder "prompt" (C.center $ txt (g ^. prompt))
    inputWidget = addBorder "input" (renderEditor (txt . T.unlines) True (g ^. input))

drawProgressBar :: Game -> Widget Name
drawProgressBar g = addBorder "progress" (P.progressBar (Just (show $ progressPercent * 100)) progressPercent)
  where
    progressPercent = ((/) `F.on` fromIntegral) (T.length (T.unwords $ E.getEditContents $ g ^. input)) (T.length (g ^. prompt))

addBorder :: T.Text -> Widget Name -> Widget Name
addBorder t = withBorderStyle BS.unicodeRounded . B.borderWithLabel (txt t)

handleKey :: Game -> BrickEvent Name e -> EventM Name (Next Game)
handleKey g (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt g
    _ -> handleEventLensed g input E.handleEditorEvent ev >>= M.continue
handleKey st _ = M.continue st

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (P.progressCompleteAttr, V.black `on` V.white),
      (P.progressIncompleteAttr, V.white `on` V.black)
    ]

theApp :: M.App Game e Name
theApp =
  M.App
    { M.appDraw = draw,
      M.appChooseCursor = showFirstCursor,
      M.appHandleEvent = handleKey,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }

run :: IO ()
run = do
  _ <- M.defaultMain theApp initializeGame
  return ()