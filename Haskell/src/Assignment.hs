module Assignment (markdownParser, convertADTHTML) where

import Control.Applicative
  ( Alternative (many, some, (<|>)),
    Applicative (liftA2),
  )
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Instances (Parser (..))
import Parser (char, digit, is, isNot, oneof, pBetween, pUntil, spaces, spaces1, string)

data Md = Md [FreeText]
  -- Your ADT **must** derive Show.
  deriving (Show, Eq)

data FreeText = FreeText [ModText]
  deriving (Show, Eq)

data ModText
  = Bold [ModText]
  | Italic [ModText]
  | StrikeThrough [ModText]
  | InlineCode String
  | Untouched Char
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

data FootnoteReference = FootnoteReference (ModText, String)
  deriving (Show, Eq)

-- PARSE FREETEXT
makeModParser :: ([a] -> ModText) -> Parser a -> Parser b -> Parser ModText
makeModParser f p1 p2 = f <$> p1 `pBetween` p2

pBold :: Parser ModText
pBold = makeModParser Bold pModText (string "**")

pItalic :: Parser ModText
pItalic = makeModParser Italic pModText (string "_")

pStrikeThrough :: Parser ModText
pStrikeThrough = makeModParser StrikeThrough pModText (string "~~")

pInlineCode :: Parser ModText
pInlineCode = makeModParser InlineCode (isNot '\n') (string "`")

pLink :: Parser ModText
pLink = Link <$> linkTuple
  where
    linkTuple = liftA2 (,) linkText linkURL
    linkText = is '[' *> isNot '\n' `pUntil` is ']'
    linkURL = is '(' *> isNot '\n' `pUntil` is ')'

pFootnote :: Parser ModText
pFootnote = Footnote <$> (string "[^" *> digit `pUntil` is ']')

pUntouched :: Parser ModText
pUntouched = Untouched <$> isNot '\n'

pModText :: Parser ModText
pModText = pBold <|> pItalic <|> pStrikeThrough <|> pInlineCode <|> pFootnote <|> pLink <|> pUntouched

pFreeText :: Parser FreeText
pFreeText = FreeText <$> many pModText

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

--- FOOTNOTE REFERENCES
pFootnoteReference :: Parser FootnoteReference
pFootnoteReference = FootnoteReference <$> refContent
  where
    refContent = liftA2 (,) theReference theContent
    theReference = pFootnote <* is ':'
    theContent = spaces *> many (isNot '\n')

-- HEADINGS

-- LINES
-- MARKDOWN
markdownParser :: Parser Md
markdownParser = Md . (: []) <$> pFreeText

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: Md -> String
convertADTHTML _ = "else"
