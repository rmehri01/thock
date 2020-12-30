{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Quotes where

import Control.Lens (makeLenses)
import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier),
    ToJSON (toJSON),
    decodeStrict,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import Data.FileEmbed (embedFile)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Random (Random (randomRIO))

data Quote = Quote
  { -- | The quote text itself
    _text :: T.Text,
    -- | Where the quote came from
    _source :: T.Text,
    -- | The number of characters in the quote
    _numChars :: Int
  }
  deriving (Generic)

makeLenses ''Quote

instance FromJSON Quote where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

instance ToJSON Quote where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

-- | Produces a random 'Quote' using a JSON file
generateQuote :: IO Quote
generateQuote = randomElem qs
  where
    qs = fromMaybe (error "could not decode JSON into quote") mqs
    mqs = decodeStrict $(embedFile "resources/quotes.json")

-- | Produces a random element in the given list
randomElem :: [a] -> IO a
randomElem xs = (xs !!) <$> randomRIO (0, length xs - 1)
