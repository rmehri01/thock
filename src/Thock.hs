{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thock where

import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import Lens.Micro.TH

data Name = Thock deriving (Ord, Show, Eq)

data Game = Game
  { _prompt :: T.Text,
    _input :: E.Editor T.Text Name
  }

makeLenses ''Game

initializeGame :: Game
initializeGame =
  Game
    "Placeholder prompt!"
    (E.editor Thock (Just 1) "")
