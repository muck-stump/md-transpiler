module Assignment (markdownParser, convertADTHTML) where

import Control.Applicative
  ( Alternative (many, some, (<|>)),
    Applicative (liftA2),
    liftA3,
  )
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.TypeLits (Mod)
import Instances (Parser (..))
import Parser (char, digit, eof, int, is, isNot, oneof, pBetween, pUntil, spaces, spaces1, string)

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
  | Footnote Int -- bad, enforce int-ness
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

-- stuff?
pBasic :: Parser Char
pBasic = isNot '\n'

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
pInlineCode = makeModParser InlineCode pBasic (string "`")

pLink :: Parser ModText
pLink = Link <$> linkTuple
  where
    linkTuple = liftA2 (,) linkText linkURL
    linkText = is '[' *> pBasic `pUntil` is ']'
    linkURL = is '(' *> pBasic `pUntil` is ')'

pFootnote :: Parser ModText
pFootnote = Footnote <$> (string "[^" *> int <* is ']')

pUntouched :: Parser ModText
pUntouched = Untouched <$> pBasic

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
    altTextContent = string "![" *> pBasic `pUntil` is ']'

pImgURL :: Parser ImgURL
pImgURL = ImgURL <$> imgURLContent
  where
    imgURLContent = is '(' *> pBasic `pUntil` is ' ' <* spaces1

pCaptionText :: Parser CaptionText
pCaptionText = CaptionText <$> captionTextContent
  where
    captionTextContent = pBasic `pBetween` is '\"'

--- FOOTNOTE REFERENCES
pFootnoteReference :: Parser FootnoteReference
pFootnoteReference = FootnoteReference <$> refContent
  where
    refContent = liftA2 (,) theReference theContent
    theReference = pFootnote <* is ':'
    theContent = spaces *> many pBasic

-- HEADINGS

-- LINES

-- MARKDOWN
markdownParser :: Parser Md
markdownParser = Md <$> (pFreeText <* is '\n') `pUntil` eof

-- markdownParser = Md <$> ((:) <$> pFreeText <* is '\n' <*> ((: []) <$> pFreeText))

-- printing classing and typing
class PrettyBoy p where
  preText :: p -> String
  preText _ = ""

  postText :: p -> String
  postText _ = ""

  prettyPrint :: String -> p -> String
  prettyPrint d p = preText p ++ unwrapPrint d p ++ postText p

  unwrapPrint :: String -> p -> String

  prettyPrintAux :: String -> [p] -> String
  prettyPrintAux delim = foldl accumulate ""
    where
      accumulate acc text = acc ++ delim ++ prettyPrint delim text

instance PrettyBoy Md where
  preText :: Md -> String
  preText _ = "this is before Md"

  postText :: Md -> String
  postText _ = "this is after Md"

  unwrapPrint :: String -> Md -> String
  unwrapPrint d (Md a) = prettyPrintAux sep a ++ d
    where
      sep = d ++ "    "

instance PrettyBoy FreeText where
  preText :: FreeText -> String
  preText _ = "<p>"

  postText :: FreeText -> String
  postText _ = "</p>"

  unwrapPrint :: String -> FreeText -> String
  unwrapPrint _ (FreeText a) = prettyPrintAux "" a

-- unwrapPrint d (FreeText a) = sep ++ prettyPrintAux "" a ++ d
--   where
--     sep = d ++ "    "

instance PrettyBoy ModText where
  preText :: ModText -> String
  preText (Bold _) = "<strong>"
  preText (Italic _) = "<em>"
  preText (StrikeThrough _) = "<del>"
  preText (InlineCode _) = "<code>"
  preText (Untouched _) = ""
  preText (Link _) = "<strong>"
  preText (Footnote _) = "<strong>"

  postText :: ModText -> String
  postText (Bold _) = "</strong>"
  postText (Italic _) = "</em>"
  postText (StrikeThrough _) = "</del>"
  postText (InlineCode _) = "</code>"
  postText (Untouched _) = ""
  postText (Link _) = "</strong>"
  postText (Footnote _) = "</strong>"

  unwrapPrint :: String -> ModText -> String
  unwrapPrint _ (Bold a) = prettyPrintAux "" a
  unwrapPrint _ (Italic a) = prettyPrintAux "" a
  unwrapPrint _ (StrikeThrough a) = prettyPrintAux "" a
  unwrapPrint _ (InlineCode a) = a
  unwrapPrint _ (Untouched a) = [a]
  unwrapPrint _ (Link a) = show a
  unwrapPrint _ (Footnote a) = show a

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: Md -> String
convertADTHTML = prettyPrint "\n"
