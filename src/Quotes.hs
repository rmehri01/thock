{-# LANGUAGE DeriveGeneric #-}

module Quotes where

import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import GHC.Generics
import System.Random

data Quote = Quote
  { text :: T.Text,
    source :: T.Text,
    length :: Int
  }
  deriving (Show, Generic)

instance FromJSON Quote

generateQuote :: IO T.Text
generateQuote = do
  quotes <- decodeFileStrict "resources/quotes.json"
  randomIndex <- randomRIO (3, Prelude.length $ fromJust quotes)
  return $ text $ fromJust quotes !! randomIndex
