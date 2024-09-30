module Assignment (markdownParser, convertADTHTML) where

import Control.Applicative
  ( Alternative (many, some, (<|>)),
    Applicative (liftA2),
  )
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Instances (Parser (..))
import Parser (char, digit, is, isNot, oneof, pBetween, pUntil, spaces1, string)

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
  | Link (String, String)
  | Footnote String -- bad, enforce int-ness
  deriving (Show, Eq)

data Image = Image AltText ImgURL CaptionText
  deriving (Show, Eq)

data AltText = AltText String
  deriving (Show, Eq)

data ImgURL = ImgURL String
  deriving (Show, Eq)

data CaptionText = CaptionText String
  deriving (Show, Eq)

-- pMod :: String -> Parser ModText
-- pMod Mod Inp = Mod <$> char `pBetween` string Inp

-- PARSE FREETEXT
makeModParser :: ([a] -> ModText) -> Parser a -> Parser b -> Parser ModText
makeModParser f p1 p2 = f <$> p1 `pBetween` p2

pBold :: Parser ModText
pBold = makeModParser Bold (isNot '\n') (string "**")

pItalic :: Parser ModText
pItalic = makeModParser Italic (isNot '\n') (string "_")

pStrikeThrough :: Parser ModText
pStrikeThrough = makeModParser StrikeThrough (isNot '\n') (string "~~")

pInlineCode :: Parser ModText
pInlineCode = makeModParser StrikeThrough (isNot '\n') (string "`")

pLink :: Parser ModText
pLink = Link <$> linkTuple
  where
    linkTuple = liftA2 (,) linkText linkURL
    linkText = is '[' *> isNot '\n' `pUntil` is ']'
    linkURL = is '(' *> isNot '\n' `pUntil` is ')'

pFootnote :: Parser ModText
pFootnote = Footnote <$> (string "[^" *> digit `pUntil` is ']')

pModText :: Parser ModText
pModText = pBold <|> pItalic <|> pStrikeThrough <|> pInlineCode <|> pFootnote <|> pLink

pUntouched :: Parser ModText
pUntouched = Untouched <$> isNot '\n' `pUntil` pModText

pFreeText :: Parser FreeText
pFreeText = FreeText <$> many (pUntouched <|> pModText)

-- PARSE IMAGES
pImage :: Parser Image
pImage = Image <$> pAltText <*> pImgURL <*> pCaptionText

pAltText :: Parser AltText
pAltText = AltText <$> altTextContent
  where
    altTextContent = string "![" *> isNot '\n' `pUntil` is ']'

pImgURL :: Parser ImgURL
pImgURL = ImgURL <$> imgURLContent
  where
    imgURLContent = is '(' *> isNot '\n' `pUntil` is ' ' <* spaces1

pCaptionText :: Parser CaptionText
pCaptionText = CaptionText <$> captionTextContent
  where
    captionTextContent = isNot '\n' `pBetween` is '\"'

markdownParser :: Parser Md
markdownParser = Md . (: []) <$> pFreeText

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: Md -> String
convertADTHTML (Md [FreeText [Bold the_parsed_thing]]) = the_parsed_thing
convertADTHTML _ = "else"
