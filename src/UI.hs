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
handleKey g = undefined

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (E.editAttr, V.white `on` V.blue),
      (E.editFocusedAttr, V.black `on` V.yellow)
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