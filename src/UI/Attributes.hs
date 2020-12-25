module UI.Attributes where

import qualified Brick.AttrMap as A
import Brick.Forms
import Brick.Util
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.ProgressBar as P
import qualified Graphics.Vty as V

primaryAttr :: A.AttrName
primaryAttr = A.attrName "primary"

primaryColor :: V.Color
primaryColor = V.rgbColor 186 255 (201 :: Int)

secondaryAttr :: A.AttrName
secondaryAttr = A.attrName "secondary"

secondaryColor :: V.Color
secondaryColor = V.rgbColor 255 255 (186 :: Int)

correctAttr :: A.AttrName
correctAttr = A.attrName "correct"

incorrectAttr :: A.AttrName
incorrectAttr = A.attrName "incorrect"

titleAttr :: A.AttrName
titleAttr = A.attrName "title"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (primaryAttr, fg primaryColor),
      (secondaryAttr, fg secondaryColor),
      (L.listAttr, fg V.white),
      (L.listSelectedAttr, fg secondaryColor `V.withStyle` V.bold),
      (P.progressCompleteAttr, V.black `on` V.white),
      (P.progressIncompleteAttr, V.white `on` V.black),
      (correctAttr, fg V.green),
      (incorrectAttr, bg V.red),
      (titleAttr, fg primaryColor),
      (E.editFocusedAttr, V.black `on` V.yellow),
      (invalidFormInputAttr, V.white `on` V.red),
      (focusedFormInputAttr, V.black `on` V.yellow)
    ]
