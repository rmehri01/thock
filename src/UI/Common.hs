{-# LANGUAGE TemplateHaskell #-}

-- | This module has common functionality that is used in both the offline and online UIs.
module UI.Common where

import Brick
  ( EventM,
    Size (Fixed),
    Widget (Widget, render),
    availWidthL,
    emptyWidget,
    fill,
    getContext,
    hBox,
    hLimit,
    hLimitPercent,
    handleEventLensed,
    str,
    txt,
    txtWrap,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
    (<+>),
    (<=>),
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.ProgressBar as P
import Control.Lens ((&), (+~), (^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.FileEmbed (embedFile)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (getCurrentTime)
import Graphics.Vty (Event)
import Quotes (source, text)
import Text.Printf (PrintfArg, printf)
import Thock
  ( GameState,
    ResourceName,
    accuracy,
    calculateProgress,
    calculateWpm,
    input,
    isDone,
    movePromptCursor,
    numCorrectChars,
    numIncorrectChars,
    quote,
    secondsElapsed,
    strokes,
    updateTime,
  )
import UI.Attributes
  ( correctAttr,
    incorrectAttr,
    primaryAttr,
    secondaryAttr,
  )

-- | Produces a styled title widget based on the logo file
titleWidget :: Widget n
titleWidget = withAttr primaryAttr logo
  where
    logo = vBox . map txt . T.lines $ decodeUtf8 $(embedFile "resources/logo.txt")

-- | Draws an element of a list based on if it is selected
listDrawElement :: Bool -> T.Text -> Widget n
listDrawElement sel t = C.hCenter $ txt symbol <+> txt t
  where
    symbol =
      if sel
        then "* "
        else "  "

-- | Draws a widget with the player's stats and further
-- instructions if the game is done, otherwise is empty
drawFinished :: GameState -> T.Text -> Widget ResourceName
drawFinished g help = if isDone g then doneWidget else emptyWidget
  where
    doneWidget = C.centerLayer . hLimitPercent 90 $ addBorder "stats" (stats <=> B.hBorder <=> instructions)
    stats = speedStat <=> accuracyStat <=> timeStat <=> sourceStat
    speedStat = makeStatWidget "Speed: " (drawWpm (calculateWpm g))
    accuracyStat = makeStatWidget "Accuracy: " (drawFloatWithSuffix 1 "%" (accuracy g * 100))
    timeStat = makeStatWidget "Time elapsed: " (drawFloatWithSuffix 1 " seconds" (secondsElapsed g))
    sourceStat = makeStatWidget "Quote source: " (txtWrap $ g ^. (quote . source))
    instructions = C.hCenter (txt help)
    makeStatWidget t w = vLimit 1 (hLimit 15 (withAttr secondaryAttr (txt t) <+> fill ' ') <+> w)

-- | Draws a progress bar for a local game
drawProgressBarGameState :: GameState -> Widget ResourceName
drawProgressBarGameState g = drawProgressBar (calculateProgress g) (calculateWpm g) "your progress"

-- | Creates a progress bar widget using the given decimal amount done and WPM
drawProgressBar :: Float -> Double -> T.Text -> Widget ResourceName
drawProgressBar decimalDone wpm label = progressWidget <+> wpmWidget
  where
    progressWidget = addBorder label (P.progressBar (Just percentStr) decimalDone)
    percentStr = roundToStr 1 (decimalDone * 100) ++ "%"
    wpmWidget = addBorder "" (drawWpm wpm)

-- | Creates the WPM widget by rounding the given number to 0 decimal points
drawWpm :: Double -> Widget ResourceName
drawWpm = drawFloatWithSuffix 0 " WPM"

-- | Rounds the given floating point number to n decimal points and attaches s as a suffix
drawFloatWithSuffix :: (PrintfArg a, Floating a) => Int -> String -> a -> Widget ResourceName
drawFloatWithSuffix n s = str . (++ s) . roundToStr n

-- | Rounds the given floating point number to an integer number of decimal points
roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

-- | Creates the prompt display for the 'GameState' with
-- highlighting based on correctly or incorrectly typed characters
drawPrompt :: GameState -> Widget ResourceName
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

-- | Produces a list of line lengths for the prompt that fit within the given lim
lineLengths :: GameState -> Int -> [Int]
lineLengths g lim =
  let go [] acc = [acc | acc /= 0]
      go allT@(t : ts) acc
        | nextAcc < lim = go ts nextAcc
        | otherwise = acc : go allT 0
        where
          nextAcc = T.length t + acc + lenSpace
          lenSpace = if null ts then 0 else 1
   in go (T.words $ g ^. (quote . text)) 0

-- | Uses a list of line lengths to combine single character widgets into
-- a single widget that is a block of text
drawTextBlock :: [Widget ResourceName] -> [Int] -> Widget ResourceName
drawTextBlock ws ls
  | null ws || null ls = emptyWidget
  | otherwise = hBox row <=> drawTextBlock rest (tail ls)
  where
    (row, rest) = splitAt (head ls) ws

-- | Draws the user input space
drawInput :: GameState -> Widget ResourceName
drawInput g = addBorder "input" (E.renderEditor (txt . T.unlines) True (g ^. input))

-- | Adds a rounded border to a widget with the given label
addBorder :: T.Text -> Widget ResourceName -> Widget ResourceName
addBorder t = withBorderStyle BS.unicodeRounded . B.borderWithLabel (txt t)

-- | Produces the next game state by applying an event and
-- updating the 'GameState' accordingly
updateGameState :: GameState -> Event -> EventM ResourceName GameState
updateGameState g ev = do
  let gWithStroke = g & strokes +~ 1
  gEdited <- handleEventLensed gWithStroke input E.handleEditorEvent ev
  currentTime <- liftIO getCurrentTime
  return . updateTime currentTime $ movePromptCursor gEdited
