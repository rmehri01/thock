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
primaryColor = V.rgbColor 64 122 (82 :: Int)

-- | The secondary attribute used for styling the UI
secondaryAttr :: A.AttrName
secondaryAttr = A.attrName "secondary"

-- | The secondary color used for styling the UI
secondaryColor :: V.Color
secondaryColor = V.rgbColor 0 100 (66 :: Int)

-- | The attribute for styling correctly typed text
correctAttr :: A.AttrName
correctAttr = A.attrName "correct"

-- | The attribute for styling incorrectly typed text
incorrectAttr :: A.AttrName
incorrectAttr = A.attrName "incorrect"

-- | Attribute for styling red text
redAttr :: A.AttrName
redAttr = A.attrName "red"

-- | Map from an attribute to its corresponding styling for all attributes
attributeMap :: A.AttrMap
attributeMap =
  A.attrMap
    V.defAttr
    [ (primaryAttr, fg primaryColor),
      (secondaryAttr, fg secondaryColor),
      (L.listSelectedAttr, fg secondaryColor `V.withStyle` V.bold),
      (P.progressCompleteAttr, V.white `on` primaryColor),
      (correctAttr, fg V.green),
      (incorrectAttr, bg V.red),
      (redAttr, fg (V.rgbColor 195 39 (43 :: Int)))
    ]
