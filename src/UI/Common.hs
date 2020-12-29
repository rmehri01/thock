{-# LANGUAGE TemplateHaskell #-}

module UI.Common where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.ProgressBar as P
import Control.Monad.IO.Class
import Data.FileEmbed
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time
import Graphics.Vty (Event)
import Lens.Micro
import Quotes
import Text.Printf
import Thock
import UI.Attributes

titleWidget :: Widget n
titleWidget = withAttr titleAttr logo
  where
    logo = vBox . map txt . T.lines $ decodeUtf8 $(embedFile "resources/logo.txt")

listDrawElement :: Bool -> T.Text -> Widget n
listDrawElement sel t = C.hCenter $ txt symbol <+> txt t
  where
    symbol =
      if sel
        then "* "
        else "  "

drawPractice :: Game -> [Widget ResourceName]
drawPractice g = [drawFinished g, drawProgressBarGame g <=> drawPrompt g <=> drawInput g]

drawFinished :: Game -> Widget ResourceName
drawFinished g = if isDone g then doneWidget else emptyWidget
  where
    doneWidget = C.centerLayer . hLimitPercent 90 $ addBorder "stats" (stats <=> B.hBorder <=> instructions)
    stats = speedStat <=> accuracyStat <=> timeStat <=> sourceStat
    speedStat = makeStatWidget "Speed: " (drawWpm (calculateWpm g))
    accuracyStat = makeStatWidget "Accuracy: " (drawFloatWithSuffix 1 "%" (accuracy g * 100))
    timeStat = makeStatWidget "Time elapsed: " (drawFloatWithSuffix 1 " seconds" (secondsElapsed g))
    sourceStat = makeStatWidget "Quote source: " (txtWrap $ g ^. (quote . source))
    instructions = C.hCenter (txt "Back: Esc | Retry: ^r | Next: ^n")
    makeStatWidget t w = vLimit 1 (hLimit 15 (withAttr secondaryAttr (txt t) <+> fill ' ') <+> w)

drawProgressBarGame :: Game -> Widget ResourceName
drawProgressBarGame g = drawProgressBar (progress g) (calculateWpm g) "your progress"

drawProgressBar :: Float -> Double -> T.Text -> Widget ResourceName
drawProgressBar decimalDone wpm label = progressWidget <+> wpmWidget
  where
    progressWidget = addBorder label (P.progressBar (Just percentStr) decimalDone)
    percentStr = roundToStr 1 (decimalDone * 100) ++ "%"
    wpmWidget = addBorder "" (drawWpm wpm)

drawWpm :: Double -> Widget ResourceName
drawWpm = drawFloatWithSuffix 0 " WPM"

drawFloatWithSuffix :: (PrintfArg a, Floating a) => Int -> String -> a -> Widget ResourceName
drawFloatWithSuffix n s = str . (++ s) . roundToStr n

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

drawPrompt :: Game -> Widget ResourceName
drawPrompt g = addBorder "prompt" (C.center $ hLimitPercent 80 textWidget)
  where
    textWidget = Widget Fixed Fixed $ do
      ctx <- getContext
      render $
        drawTextBlock
          (correctWidgets ++ incorrectWidgets ++ restWidgets)
          (lineLengths g (ctx ^. availWidthL))
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

drawTextBlock :: [Widget ResourceName] -> [Int] -> Widget ResourceName
drawTextBlock ws ls
  | null ws || null ls = emptyWidget
  | otherwise = hBox row <=> drawTextBlock rest (tail ls)
  where
    (row, rest) = splitAt (head ls) ws

drawInput :: Game -> Widget ResourceName
drawInput g = addBorder "input" (E.renderEditor (txt . T.unlines) True (g ^. input))

addBorder :: T.Text -> Widget ResourceName -> Widget ResourceName
addBorder t = withBorderStyle BS.unicodeRounded . B.borderWithLabel (txt t)

updateGame :: Game -> Event -> EventM ResourceName Game
updateGame g ev = do
  gEdited <- handleEventLensed g input E.handleEditorEvent ev
  currentTime <- liftIO getCurrentTime
  return . updateTime currentTime $ movePromptCursor gEdited
