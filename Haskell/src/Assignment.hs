module Assignment (markdownParser, convertADTHTML) where

import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Instances (Parser (..))
import Parser ()

data ADT = Empty
  -- Your ADT **must** derive Show.
  deriving (Show, Eq)

data FreeText
  = Bold String
  | Italic String
  | StrikeThrough String
  | InlineCode String
  | Untouched String
  deriving (Show, Eq)

markdownParser :: Parser ADT
markdownParser = pure Empty

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: ADT -> String
convertADTHTML Empty = "IMPLEMENT_THIS"
