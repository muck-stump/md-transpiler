module Assignment (markdownParser, convertADTHTML) where

import Control.Applicative
  ( Alternative (many, some, (<|>)),
    Applicative (liftA2),
    liftA,
    liftA3,
  )
import Control.Monad (guard)
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.IO.Encoding.Types (CodeBuffer)
import GHC.TypeLits (Mod)
import Instances (Parser (..))
import Parser (char, charTok, digit, eof, inlineSpace, int, is, isNot, newline, oneof, pBetween, pUntil, roken, sepBy, sepBy1, space, spaces, spaces1, string, trailEnd)
import Types (FreeText)

data Md = Md [Block]
  -- Your ADT **must** derive Show.
  deriving (Show, Eq)

data Block
  = FreeText [ModText]
  | Image AltText ImgURL CaptionText
  | FootnoteReference Int String
  | Quote [Line]
  | Code Language Program
  | Heading Int [ModText]
  deriving (Show, Eq)

data Language = Language String
  deriving (Show, Eq)

data Program = Program String
  deriving (Show, Eq)

data CodeLine = CodeLine String
  deriving (Show, Eq)

data Line = Line [ModText]
  deriving (Show, Eq)

-- ask how to make this seperate types
data ModText
  = Bold [ModText]
  | Italic [ModText]
  | StrikeThrough [ModText]
  | InlineCode String
  | Untouched Char
  | Link (String, String)
  | Footnote Int
  deriving (Show, Eq)

data AltText = AltText String
  deriving (Show, Eq)

data ImgURL = ImgURL String
  deriving (Show, Eq)

data CaptionText = CaptionText String
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

pFreeText :: Parser Block
pFreeText = pad $ FreeText <$> some pModText

-- PARSE IMAGES
pImage :: Parser Block
pImage = pad $ liftA3 Image pAltText pImgURL pCaptionText

pAltText :: Parser AltText
pAltText = AltText <$> altTextContent
  where
    altTextContent = string "![" *> pBasic `pUntil` is ']'

pImgURL :: Parser ImgURL
pImgURL = ImgURL <$> imgURLContent
  where
    imgURLContent = is '(' *> pBasic `pUntil` is ' ' <* spaces -- validate the no space thing i guess?

pCaptionText :: Parser CaptionText
pCaptionText = CaptionText <$> captionTextContent
  where
    captionTextContent = pBasic `pBetween` is '\"' <* is ')'

--- FOOTNOTE REFERENCES
pFootnoteReference :: Parser Block
pFootnoteReference = pad $ liftA2 FootnoteReference theReference theContent
  where
    theReference = roken $ (string "[^" *> int <* is ']') <* is ':'
    theContent = roken $ many pBasic

-- BLOCK QUOTES
pQuote :: Parser Block
pQuote = pad $ Quote <$> (line `sepBy1` is '\n')
  where
    line = Line <$> (inlineSpace *> charTok '>' *> many pModText)

-- CODE BLOCK
pCode :: Parser Block
pCode = pad $ liftA2 Code language program
  where
    language = Language <$> (string "```" *> (pBasic `pUntil` (inlineSpace *> newline)))
    program = Program <$> (char `pUntil` (spaces *> string "```"))

-- HEADINGS

pHeading :: Parser Block
pHeading = pad $ heading <|> h1alt <|> h2alt
  where
    heading = do
      _ <- spaces
      depth <- length <$> some (is '#')
      _ <- guard (depth < 7)
      _ <- some space
      Heading depth <$> some pModText
    h1alt = pHeadingAux 1 (is '=')
    h2alt = pHeadingAux 2 (is '-')

pHeadingAux :: Int -> Parser a -> Parser Block
pHeadingAux depth = (Heading depth <$>) . text
  where
    text d = roken $ some pModText <* delimiter d
    delimiter d = is '\n' <* roken d <* some d <* inlineSpace

-- PADDING
-- before the block is MANY number of whitespaces of any kind
-- after the block is SOME whitespace (any kind, at least 1 newline) or MANY whitespace then eof

pad :: Parser a -> Parser a
pad = padBefore . padAfter
  where
    padBefore = (*>) spaces
    padAfter = (<* (midPad <|> lastPad))
    midPad = inlineSpace *> newline *> inlineSpace
    lastPad = spaces <* eof

-- BLOCK
pBlock :: Parser Block
pBlock = pHeading <|> pCode <|> pQuote <|> pFootnoteReference <|> pImage <|> pFreeText

-- MARKDOWN
markdownParser :: Parser Md
markdownParser = Md <$> (many pBlock)

-- printing classing and typing
class PrettyBoy p where
  preText :: String -> p -> String
  preText _ _ = ""

  postText :: String -> p -> String
  postText _ _ = ""

  prettyPrint :: String -> p -> String
  prettyPrint = combine3 preText unwrapPrint postText
    where
      combine3 = dliftA3 concat3 -- double liftA3, and combine by sticking the three stings together
      dliftA3 = liftA3 . liftA3 -- double liftA3. use to thread 2 arguments through 3 functions, then combine
      concat3 = ((++) .) . (++) -- take three strings and stick them together

  unwrapPrint :: String -> p -> String

  prettyPrintAux :: String -> [p] -> String
  prettyPrintAux delim res = intercalate delim (prettyPrint delim <$> res)

-- prettyPrintAux delim = foldl accumulate ""
--   where
--     accumulate acc text = acc ++ delim ++ prettyPrint delim text

instance PrettyBoy Md where
  preText :: String -> Md -> String
  preText delim _ = "<!DOCTYPE html>" ++ delim ++ "<html lang=\"en\">" ++ delim ++ delim ++ "<head>" ++ delim ++ "    <meta charset=\"UTF-8\">" ++ delim ++ "    <title>Test</title>" ++ delim ++ "</head>" ++ delim ++ delim ++ "<body>" ++ delim ++ "    "

  postText :: String -> Md -> String
  postText delim _ = "</body>" ++ delim ++ delim ++ "</html>" ++ delim

  unwrapPrint :: String -> Md -> String
  unwrapPrint d (Md a) = prettyPrintAux sep a ++ d
    where
      sep = d ++ "    "

instance PrettyBoy Block where
  preText :: String -> Block -> String
  preText _ Image {} = "<p><img "
  preText _ (FootnoteReference n _) = "<p id=\"fn" ++ show n ++ "\">"
  preText delim (Quote _) = "<blockquote>" ++ delim ++ "    "
  preText _ (Code (Language lang) _) = "<pre><code class=\"language-" ++ lang ++ "\">"
  preText _ (Heading depth _) = "<h" ++ show depth ++ ">"
  preText _ _ = "<p>"

  postText :: String -> Block -> String
  postText _ Image {} = "></p>"
  postText _ (Quote _) = "</blockquote>"
  postText _ (Code {}) = "</code></pre>"
  postText _ (Heading depth _) = "</h" ++ show depth ++ ">"
  postText _ _ = "</p>"

  unwrapPrint :: String -> Block -> String
  unwrapPrint d (Quote a) = prettyPrintAux sep a ++ d
    where
      sep = d ++ "    "
  unwrapPrint _ (FootnoteReference _ txt) = txt
  unwrapPrint _ (Image (AltText alt) (ImgURL url) (CaptionText txt)) = "src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\" title=\"" ++ txt ++ "\""
  unwrapPrint _ (FreeText a) = prettyPrintAux "" a
  unwrapPrint _ (Code _ (Program prog)) = prog
  unwrapPrint _ (Heading _ text) = prettyPrintAux "" text

-- unwrapPrint _ _ = ""

-- unwrapPrint d (FreeText a) = sep ++ prettyPrintAux "" a ++ d
--   where
--     sep = d ++ "    "

instance PrettyBoy CodeLine where
  unwrapPrint :: String -> CodeLine -> String
  unwrapPrint _ (CodeLine str) = str

instance PrettyBoy Line where
  preText :: String -> Line -> String
  preText _ _ = "<p>"

  postText :: String -> Line -> String
  postText _ _ = "</p>"

  unwrapPrint :: String -> Line -> String
  unwrapPrint _ (Line a) = prettyPrintAux "" a

instance PrettyBoy ModText where
  preText :: String -> ModText -> String
  preText _ (Bold _) = "<strong>"
  preText _ (Italic _) = "<em>"
  preText _ (StrikeThrough _) = "<del>"
  preText _ (InlineCode _) = "<code>"
  preText _ (Untouched _) = ""
  preText _ (Link _) = "<a href=\""
  preText _ (Footnote _) = "<sup>"

  postText :: String -> ModText -> String
  postText _ (Bold _) = "</strong>"
  postText _ (Italic _) = "</em>"
  postText _ (StrikeThrough _) = "</del>"
  postText _ (InlineCode _) = "</code>"
  postText _ (Untouched _) = ""
  postText _ (Link _) = "</a>"
  postText _ (Footnote _) = "</sup>"

  unwrapPrint :: String -> ModText -> String
  unwrapPrint _ (Bold a) = prettyPrintAux "" a
  unwrapPrint _ (Italic a) = prettyPrintAux "" a
  unwrapPrint _ (StrikeThrough a) = prettyPrintAux "" a
  unwrapPrint _ (InlineCode a) = a
  unwrapPrint _ (Untouched a) = [a]
  unwrapPrint _ (Link a) = snd a ++ "\">" ++ fst a
  unwrapPrint _ (Footnote a) = "<a id=\"fn" ++ n ++ "ref\" href=\"#fn" ++ n ++ "\">" ++ n ++ "</a>"
    where
      n = show a

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: Md -> String
convertADTHTML = prettyPrint "\n"
