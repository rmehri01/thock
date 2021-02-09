{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module provides a way to deal with quotes which are used as prompts.
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
import Data.Bifunctor (second)
import Data.FileEmbed (embedDir, embedFile)
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Random (Random (randomRIO))

-- | The choosen set of quotes
data QuotesSet
  = English
  | Russian
  | Haskell
  deriving (Eq, Generic, Show)

instance FromJSON QuotesSet

instance ToJSON QuotesSet

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
generateQuote :: QuotesSet -> IO Quote
generateQuote set = randomElem qs
  where
    qs = flip fromMaybe fqs $ error "could not decode JSON into quote"
    fqs = findSet set mqs
    mqs = map (second decodeStrict) $(embedDir "resources/quotes")

-- | Returns the first element that matches the path
-- for the given `QuotesSet`
findSet :: QuotesSet -> [(FilePath, Maybe [a])] -> Maybe [a]
findSet qs xs = find ((== getPath qs) . fst) xs >>= snd
  where
    getPath set = case set of
      English -> "quotes_en.json"
      Russian -> "quotes_ru.json"
      Haskell -> "quotes_hask.json"

-- | Produces a random element in the given list
randomElem :: [a] -> IO a
randomElem xs = (xs !!) <$> randomRIO (0, length xs - 1)
