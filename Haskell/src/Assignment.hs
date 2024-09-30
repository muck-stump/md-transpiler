module Assignment (markdownParser, convertADTHTML) where

import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Instances (Parser (..))
import Parser (char, pBetween, string)

data Md = Md [FreeText]
  -- Your ADT **must** derive Show.
  deriving (Show, Eq)

data FreeText = FreeText [ModText]
  deriving (Show, Eq)

data ModText
  = Bold String
  | Italic String
  | StrikeThrough String
  | InlineCode String
  | Untouched String
  deriving (Show, Eq)

-- pMod :: String -> Parser ModText
-- pMod Mod Inp = Mod <$> char `pBetween` string Inp

pBold :: Parser ModText
pBold = Bold <$> char `pBetween` string "**"

-- pItalic :: Parser ModText
-- pItalic = Italic <$>

pFreeText :: Parser FreeText
pFreeText = FreeText . (: []) <$> pBold

markdownParser :: Parser Md
markdownParser = Md . (: []) <$> pFreeText

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: Md -> String
convertADTHTML (Md [FreeText [Bold the_parsed_thing]]) = the_parsed_thing
convertADTHTML _ = "else"
