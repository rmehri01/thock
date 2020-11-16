{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Quotes where

import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import GHC.Generics
import Lens.Micro.TH
import System.Random

data Quote = Quote
  { _text :: T.Text,
    _source :: T.Text,
    _numChars :: Int
  }
  deriving (Show, Generic)

makeLenses ''Quote

instance FromJSON Quote where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

generateQuote :: IO Quote
generateQuote =
  decodeFileStrict "resources/quotes.json"
    >>= randomElem . fromMaybe (error "could not decode JSON into quote")

randomElem :: [a] -> IO a
randomElem xs = (xs !!) <$> randomRIO (3, length xs - 1)
