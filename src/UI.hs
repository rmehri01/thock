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
import qualified Graphics.Vty as V
import Lens.Micro
import Quotes
import Thock

draw :: Game -> [Widget ()]
draw g = [drawProgressBar g <=> drawPrompt g <=> drawInput g]

drawProgressBar :: Game -> Widget ()
drawProgressBar g = addBorder "progress" (P.progressBar (Just percentStr) amountDone)
  where
    percentStr = show percentDone ++ "%"
    percentDone = floor (amountDone * 100) :: Int
    amountDone = progress g

drawPrompt :: Game -> Widget ()
drawPrompt g = addBorder "prompt" (C.center textWidget)
  where
    textWidget = drawTextBlock (correctWidgets ++ incorrectWidgets ++ restWidgets) (lineLengths g 50) -- TODO: wrap by context or maybe config?
    correctWidgets = withAttr "correct" . txt . T.singleton <$> T.unpack correctText
    incorrectWidgets = withAttr "incorrect" . txt . T.singleton <$> T.unpack incorrectText
    restWidgets = txt . T.singleton <$> T.unpack restText'
    (incorrectText, restText') = T.splitAt (numIncorrectChars g) restText
    (correctText, restText) = T.splitAt (numCorrectChars g) allText
    allText = g ^. (quote . text)

drawTextBlock :: [Widget ()] -> [Int] -> Widget ()
drawTextBlock ws ls
  | null ws || null ls = emptyWidget
  | otherwise = foldl1 (<+>) row <=> drawTextBlock rest (tail ls)
  where
    (row, rest) = splitAt (head ls) ws

drawInput :: Game -> Widget ()
drawInput g = addBorder "input" (E.renderEditor (txt . T.unlines) True (g ^. input))

addBorder :: T.Text -> Widget () -> Widget ()
addBorder t = withBorderStyle BS.unicodeRounded . B.borderWithLabel (txt t)

handleKey :: Game -> BrickEvent () e -> EventM () (Next Game)
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

theApp :: M.App Game e ()
theApp =
  M.App
    { M.appDraw = draw,
      M.appChooseCursor = showFirstCursor,
      M.appHandleEvent = handleKey,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }
