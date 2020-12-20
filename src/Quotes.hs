{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Quotes where

import Data.Aeson
import Data.FileEmbed
import Data.Maybe
import qualified Data.Text as T
import GHC.Generics
import Lens.Micro.TH
import qualified Network.WebSockets as WS
import System.Random

data Quote = Quote
  { _text :: T.Text,
    _source :: T.Text,
    _numChars :: Int
  }
  deriving (Generic)

makeLenses ''Quote

instance WS.WebSocketsData Quote where
  fromDataMessage d = case d of
    WS.Text b _ -> fromJust $ decode b -- TODO use mt
    WS.Binary b -> fromJust $ decode b
  fromLazyByteString = fromJust . decode -- TODO: sus
  toLazyByteString = encode

instance FromJSON Quote where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

instance ToJSON Quote where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1} -- TODO: might not have to do

generateQuote :: IO Quote
generateQuote = randomElem qs
  where
    qs = fromMaybe (error "could not decode JSON into quote") mqs
    mqs = decodeStrict $(embedFile "resources/quotes.json")

randomElem :: [a] -> IO a
randomElem xs = (xs !!) <$> randomRIO (0, length xs - 1)