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
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Random (Random (randomRIO))

-- | The choosen set of quotes
data QuotesSet
  = English
  | Russian
  | Haskell
  deriving (Eq, Generic)

instance FromJSON QuotesSet
instance ToJSON QuotesSet
instance Show QuotesSet where
  show English = "English"
  show Russian = "Russian"
  show Haskell = "Haskell"

data Quote = Quote
  { -- | The quote text itself
    _text :: T.Text,
    -- | Where the quote came from
    _source :: T.Text,
    -- | The number of characters in the quote
    _numChars :: Int
  } deriving (Generic)

makeLenses ''Quote

instance FromJSON Quote where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}
instance ToJSON Quote where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

-- | Produces a random 'Quote' using a JSON file
-- TODO: maybe it would be nice to add a general name for the pack of quotes
-- with the purpose to have a unified name to display in menu
-- and at the same time hardcoded in json as well
-- e.g. ["English": { text: "foo", source: ..., numChars: ... }, "Haskell": { ... }, ... ]
generateQuote :: QuotesSet -> IO Quote
generateQuote set = randomElem qs
  where
    qs = flip fromMaybe fqs $ error "could not decode JSON into quote"
    fqs = getFirstMatch set mqs
    mqs = map (second decodeStrict) $(embedDir "resources/quotes") :: [(FilePath, Maybe [Quote])]

-- | Returns the first element that matches the given `QuotesSet`
getFirstMatch :: QuotesSet -> [(FilePath, Maybe [a])] -> Maybe [a]
getFirstMatch qs (x:xs) =
  if fst x == path qs
  then snd x
  else getFirstMatch qs xs

  where path set = case set of
          English -> "quotes_en.json"
          Russian -> "quotes_ru.json"
          Haskell -> "quotes_hask.json"
getFirstMatch _ [] = Nothing

-- | Produces a random element in the given list
randomElem :: [a] -> IO a
randomElem xs = (xs !!) <$> randomRIO (0, length xs - 1)
