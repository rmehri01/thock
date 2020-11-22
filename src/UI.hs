{-# LANGUAGE OverloadedStrings #-}

module UI where

import           Brick
import qualified Brick.AttrMap              as A
import qualified Brick.Main                 as M
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Brick.Widgets.Edit         as E
import qualified Brick.Widgets.List         as L
import qualified Brick.Widgets.ProgressBar  as P
import           Control.Monad.IO.Class
import qualified Data.Text                  as T
import           Graphics.Vty               (rgbColor)
import qualified Graphics.Vty               as V
import           Lens.Micro
import           Quotes
import           Thock

draw :: GameState -> [Widget ()]
draw s = case s of
  MainMenu l -> drawMain l
  Practice g -> drawPractice g
  Online     -> drawOnline undefined

drawMain :: MenuList -> [Widget ()]
drawMain l = [addBorder "" (titleWidget <=> listWidget)]
  where
    listWidget = vLimitPercent 20 $ L.renderList listDrawElement True l


titleWidget :: Widget ()
titleWidget =
  C.center $
    withAttr "title" $
            txt "████████╗██╗  ██╗ ██████╗  ██████╗██╗  ██╗"
        <=> txt "╚══██╔══╝██║  ██║██╔═══██╗██╔════╝██║ ██╔╝"
        <=> txt "   ██║   ███████║██║   ██║██║     █████╔╝ "
        <=> txt "   ██║   ██╔══██║██║   ██║██║     ██╔═██╗ "
        <=> txt "   ██║   ██║  ██║╚██████╔╝╚██████╗██║  ██╗"
        <=> txt "   ╚═╝   ╚═╝  ╚═╝ ╚═════╝  ╚═════╝╚═╝  ╚═╝"

listDrawElement :: Bool -> T.Text -> Widget ()
listDrawElement sel t = C.hCenter $ txt symbol <+> txt t
  where
    symbol =
      if sel
        then "* "
        else "  "

drawPractice :: Game -> [Widget ()]
drawPractice g = [drawProgressBar g <=> drawPrompt g <=> drawInput g]

drawOnline :: Game -> [Widget ()]
drawOnline = undefined

drawProgressBar :: Game -> Widget ()
drawProgressBar g = addBorder "progress" (P.progressBar (Just percentStr) amountDone)
  where
    percentStr = show percentDone ++ "%"
    percentDone = floor (amountDone * 100) :: Int
    amountDone = progress g

drawPrompt :: Game -> Widget ()
drawPrompt g = addBorder "prompt" (C.center textWidget)
  where
    textWidget = drawTextBlock (correctWidgets ++ incorrectWidgets ++ restWidgets) (lineLengths g 65) -- TODO: wrap by context or maybe config?
    correctWidgets = withAttr "correct" . txt . T.singleton <$> T.unpack correctText
    incorrectWidgets = withAttr "incorrect" . txt . T.singleton <$> T.unpack incorrectText
    restWidgets = txt . T.singleton <$> T.unpack restText'
    (incorrectText, restText') = T.splitAt (numIncorrectChars g) restText
    (correctText, restText) = T.splitAt (numCorrectChars g) allText
    allText = g ^. (quote . text)

lineLengths :: Game -> Int -> [Int]
lineLengths g lim =
  let go [] acc = [acc | acc /= 0]
      go allT@(t : ts) acc
        | nextAcc < lim = go ts nextAcc
        | otherwise = acc : go allT 0
        where
          nextAcc = T.length t + acc + lenSpace
          lenSpace = if null ts then 0 else 1
   in go (T.words $ g ^. (quote . text)) 0

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

handleKey :: GameState -> BrickEvent () e -> EventM () (Next GameState)
handleKey gs ev = case gs of
  MainMenu l -> handleKeyMainMenu l ev
  Practice g -> handleKeyPractice g ev
  Online     -> handleKeyOnline undefined ev

handleKeyMainMenu :: MenuList -> BrickEvent () e -> EventM () (Next GameState)
handleKeyMainMenu l (VtyEvent e) = case e of
  V.EvKey V.KEsc []   -> M.halt (MainMenu l)
  V.EvKey V.KEnter [] -> liftIO generateQuote >>= M.continue . startGame l
  ev                  -> L.handleListEvent ev l >>= M.continue . MainMenu
handleKeyMainMenu l _ = M.continue (MainMenu l)

handleKeyPractice :: Game -> BrickEvent () e -> EventM () (Next GameState)
handleKeyPractice g (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt (Practice g) -- TODO: separate end screen
    _ -> handleEventLensed g input E.handleEditorEvent ev >>= M.continue . Practice . movePromptCursor
handleKeyPractice st _ = M.continue (Practice st)

handleKeyOnline :: Game -> BrickEvent () e -> EventM () (Next GameState)
handleKeyOnline = undefined

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listAttr, fg V.white),
      (L.listSelectedAttr, fg (rgbColor 255 255 186) `V.withStyle` V.bold),
      (P.progressCompleteAttr, V.black `on` V.white),
      (P.progressIncompleteAttr, V.white `on` V.black),
      ("correct", fg V.green),
      ("incorrect", bg V.red),
      ("title", fg (rgbColor 186 255 201))
    ]

theApp :: M.App GameState e ()
theApp =
  M.App
    { M.appDraw = draw,
      M.appChooseCursor = showFirstCursor,
      M.appHandleEvent = handleKey,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }
