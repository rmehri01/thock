{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Widgets.Edit
import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro
import Thock

draw :: Game -> [Widget Name]
draw g = [txt (g ^. prompt) <=> renderEditor (txt . T.unlines) True (g ^. input)]

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
    [ (E.editFocusedAttr, V.black `on` V.yellow)
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