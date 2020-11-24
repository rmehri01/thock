{-# LANGUAGE OverloadedStrings #-}

module UI where

import           Brick
import qualified Brick.Main                 as M
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Brick.Widgets.Edit         as E
import qualified Brick.Widgets.List         as L
import qualified Brick.Widgets.ProgressBar  as P
import           Control.Applicative
import           Control.Monad.IO.Class
import qualified Data.Text                  as T
import           Data.Time
import qualified Graphics.Vty               as V
import           Lens.Micro
import           Quotes
import           Text.Printf
import           Thock
import           UI.Attributes

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
  C.center
    . withAttr titleAttr
        $ txt "████████╗██╗  ██╗ ██████╗  ██████╗██╗  ██╗"
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
drawPractice g = [drawFinished g, drawProgressBar g <=> drawPrompt g <=> drawInput g]

drawFinished :: Game -> Widget ()
drawFinished g = if isDone g then doneWidget else emptyWidget
  where
    doneWidget = C.centerLayer . hLimitPercent 80 $ addBorder "stats" (stats <=> B.hBorder <=> instructions)
    stats = speedStat <=> accuracyStat <=> timeStat <=> sourceStat
    speedStat = txt "Speed: " <+> withAttr primaryAttr (drawWpm g)
    accuracyStat = txt "Accuracy: " <+> withAttr primaryAttr (drawFloatWithSuffix 1 "%" (accuracy g * 100))
    timeStat = txt "Time elapsed: " <+> withAttr primaryAttr (drawFloatWithSuffix 1 " seconds" (secondsElapsed g))
    sourceStat = txt "Quote source: " <+> withAttr primaryAttr (txt $ g ^. (quote . source))
    instructions = C.hCenter (txt "Back to menu: ^b | Retry quote: ^r | Next quote: ^n")

drawOnline :: Game -> [Widget ()]
drawOnline = undefined

drawProgressBar :: Game -> Widget ()
drawProgressBar g = progressWidget <+> wpmWidget
  where
    progressWidget = addBorder "progress" (P.progressBar (Just percentStr) amountDone)
    percentStr = roundToStr 1 (amountDone * 100) ++ "%"
    amountDone = progress g
    wpmWidget = addBorder "" (drawWpm g)

drawWpm :: Game -> Widget ()
drawWpm = drawFloatWithSuffix 0 " WPM" . wpm

drawFloatWithSuffix :: (PrintfArg a, Floating a) => Int -> String -> a -> Widget ()
drawFloatWithSuffix n s = str . (++ s) . roundToStr n

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

drawPrompt :: Game -> Widget ()
drawPrompt g = addBorder "prompt" (C.center textWidget)
  where
    textWidget = drawTextBlock (correctWidgets ++ incorrectWidgets ++ restWidgets) (lineLengths g 65) -- TODO: wrap by context or maybe config?
    correctWidgets = withAttr correctAttr . txt . T.singleton <$> T.unpack correctText
    incorrectWidgets = withAttr incorrectAttr . txt . T.singleton <$> T.unpack incorrectText
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
  V.EvKey V.KEnter [] -> startGameM Nothing (MainMenu l)
  ev                  -> L.handleListEvent ev l >>= M.continue . MainMenu
handleKeyMainMenu l _ = M.continue (MainMenu l)

handleKeyPractice :: Game -> BrickEvent () e -> EventM () (Next GameState)
handleKeyPractice g (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt (Practice g)
    V.EvKey (V.KChar 'b') [V.MCtrl] -> M.continue initialState
    V.EvKey (V.KChar 'r') [V.MCtrl] -> startGameM (Just $ g ^. quote) (Practice g)
    V.EvKey (V.KChar 'n') [V.MCtrl] -> startGameM Nothing (Practice g)
    V.EvKey (V.KChar _) [] -> nextState (g & strokes +~ 1)
    _ -> nextState g
  where
    nextState g' =
      if isDone g'
        then M.continue (Practice g')
        else do
          gEdited <- handleEventLensed g' input E.handleEditorEvent ev
          currentTime <- liftIO getCurrentTime
          M.continue . Practice . updateTime currentTime $ movePromptCursor gEdited
handleKeyPractice g _ = M.continue (Practice g)

handleKeyOnline :: Game -> BrickEvent () e -> EventM () (Next GameState)
handleKeyOnline = undefined

startGameM :: Maybe Quote -> GameState -> EventM () (Next GameState)
startGameM mq gs = liftIO generatedGameState >>= M.continue
  where
    generatedGameState = liftA2 startGame (maybe generateQuote pure mq) (pure gs)

theApp :: M.App GameState e ()
theApp =
  M.App
    { M.appDraw = draw,
      M.appChooseCursor = showFirstCursor,
      M.appHandleEvent = handleKey,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }
