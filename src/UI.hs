{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.ProgressBar as P
import qualified Data.Text as T
import Data.Text.Zipper
import qualified Graphics.Vty as V
import Lens.Micro
import Thock

draw :: Game -> [Widget Name]
draw g = [drawProgressBar g <=> drawPrompt g <=> drawInput g]

drawProgressBar :: Game -> Widget Name
drawProgressBar g = addBorder "progress" (P.progressBar (Just percentStr) amountDone)
  where
    percentStr = show percentDone ++ "%"
    percentDone = floor (amountDone * 100) :: Int
    amountDone = progress g

drawPrompt :: Game -> Widget Name
drawPrompt g = addBorder "prompt" (C.center textWidget)
  where
    textWidget = correctWidget <+> incorrectWidget <+> restWidget
    correctWidget = withAttr "correct" $ txt correctText
    incorrectWidget = withAttr "incorrect" $ txt incorrectText
    restWidget = txt restText'
    (incorrectText, restText') = T.splitAt (numIncorrectChars g) restText
    (correctText, restText) = T.splitAt (numCorrectChars g) allText
    allText = T.unwords $ getText $ g ^. prompt

drawInput :: Game -> Widget Name
drawInput g = addBorder "input" (E.renderEditor (txt . T.unlines) True (g ^. input))

addBorder :: T.Text -> Widget Name -> Widget Name
addBorder t = withBorderStyle BS.unicodeRounded . B.borderWithLabel (txt t)

handleKey :: Game -> BrickEvent Name e -> EventM Name (Next Game)
handleKey g (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt g
    _ -> handleEventLensed g input E.handleEditorEvent ev >>= M.continue . movePromptCursor
handleKey st _ = M.continue st

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (P.progressCompleteAttr, V.black `on` V.white),
      (P.progressIncompleteAttr, V.white `on` V.black),
      ("correct", fg V.green),
      ("incorrect", bg V.red)
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
