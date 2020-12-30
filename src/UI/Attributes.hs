module UI.Attributes where

import qualified Brick.AttrMap as A
import Brick.Util (bg, fg, on)
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.ProgressBar as P
import qualified Graphics.Vty as V

-- | The main attribute used for styling the UI
primaryAttr :: A.AttrName
primaryAttr = A.attrName "primary"

-- | The main color used for styling the UI
primaryColor :: V.Color
primaryColor = V.rgbColor 186 255 (201 :: Int)

-- | The secondary attribute used for styling the UI
secondaryAttr :: A.AttrName
secondaryAttr = A.attrName "secondary"

-- | The secondary color used for styling the UI
secondaryColor :: V.Color
secondaryColor = V.rgbColor 255 255 (186 :: Int)

-- | The attribute for styling correctly typed text
correctAttr :: A.AttrName
correctAttr = A.attrName "correct"

-- | The attribute for styling incorrectly typed text
incorrectAttr :: A.AttrName
incorrectAttr = A.attrName "incorrect"

-- | Map from an attribute to its corresponding styling for all attributes
attributeMap :: A.AttrMap
attributeMap =
  A.attrMap
    (V.withBackColor V.defAttr V.black)
    [ (primaryAttr, fg primaryColor),
      (secondaryAttr, fg secondaryColor),
      (L.listAttr, fg V.white),
      (L.listSelectedAttr, fg secondaryColor `V.withStyle` V.bold),
      (P.progressCompleteAttr, V.black `on` V.white),
      (P.progressIncompleteAttr, V.white `on` V.black),
      (correctAttr, fg V.green),
      (incorrectAttr, bg V.red)
    ]
