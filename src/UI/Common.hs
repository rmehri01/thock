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
import Data.Foldable
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time
import Graphics.Vty (Event)
import Lens.Micro
import Quotes
import Text.Printf
import Thock
import UI.Attributes

titleWidget :: Widget ()
titleWidget =
  C.center
    . withAttr titleAttr
    $ logo
  where
    logo = foldl' (<=>) emptyWidget . map txt . T.lines $ decodeUtf8 $(embedFile "resources/logo.txt")

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

updateGame :: Game -> Event -> EventM () Game
updateGame g ev = do
  gEdited <- handleEventLensed g input E.handleEditorEvent ev
  currentTime <- liftIO getCurrentTime
  return . updateTime currentTime $ movePromptCursor gEdited
