module Assignment (markdownParser, convertADTHTML, getTime) where

import Control.Applicative
  ( Alternative (many, some, (<|>)),
    Applicative (liftA2),
    liftA2,
    liftA3,
  )
import Control.Monad (guard, replicateM, void)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Instances (Parser (..))
import Parser (char, charTok, digit, eof, inlineSpace, int, is, isNot, newline, oneof, pBetween, pUntil, roken, sepBy, sepBy1, space, spaces, spaces1, string, trailEnd)
import PrettyBoy (PrettyBoy (prettyPrint))
import Types
  ( AltText (..),
    Block (..),
    Body (..),
    CaptionText (..),
    Cell (..),
    CodeLine (..),
    Head (..),
    HeadCell (..),
    HeadRow (..),
    Html (..),
    ImgURL (..),
    Language (Language),
    Line (..),
    Md (..),
    ModText (..),
    Program (Program),
    Row (..),
  )

-- stuff?
pBasic :: Parser Char
pBasic = isNot '\n'

-- PARSE FREE TEXT
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

pSuperScript :: Parser ModText
pSuperScript = Super <$> (is '\\' *> pModText `pUntil` is '/')

pSub :: Parser ModText
pSub = Sub <$> (is '/' *> pModText `pUntil` is '\\')

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
pModText = pBold <|> pItalic <|> pStrikeThrough <|> pSuperScript <|> pSub <|> pInlineCode <|> pFootnote <|> pLink <|> pUntouched

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
    imgURLContent = is '(' *> pBasic `pUntil` spaces1

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
pQuote = pad $ Quote <$> (line `sepBy1` newline)
  where
    line = Line <$> (inlineSpace *> charTok '>' *> many pModText)

-- CODE BLOCK
pCode :: Parser Block
pCode = pad $ liftA2 Code language program
  where
    language = Language <$> (string "```" *> (pBasic `pUntil` (inlineSpace *> newline)))
    program = Program <$> (char `pUntil` (newline *> string "```"))

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
    h2alt = pHeadingAux 2 dash

pHeadingAux :: Int -> Parser a -> Parser Block
pHeadingAux depth = (Heading depth <$>) . text
  where
    text d = roken $ some pModText <* delimiter d
    delimiter d = is '\n' <* roken d <* some d <* inlineSpace

-- TABLES

pHeadRow :: Parser Head
pHeadRow = Head <$> (HeadRow <$> (pipe *> some cells))
  where
    cells = HeadCell <$> (pModText `pUntil` (inlineSpace *> pipe))

pSeparator :: Int -> Parser () --- at least 3 '-' in each section
pSeparator = pad . (*>) pipe . separator --- same number of columns as head
  where
    separator = void <$> (`replicateM` line)
    line = cell *> dash `pUntil` pipe
    cell = replicateM 3 dash

pRow :: Int -> Parser Row
pRow = pad . (*>) pipe . row
  where
    row = (Row <$>) <$> (`replicateM` cells)
    cells = Cell <$> (pModText `pUntil` (inlineSpace *> pipe))

pipe :: Parser Char
pipe = charTok '|'

dash :: Parser Char
dash = charTok '-'

pTable :: Parser Block
pTable = do
  headRow <- pHeadRow
  let width = headLength headRow
  _ <- pSeparator width
  rows <- many (pRow width)
  return (Table headRow (Body rows))
  where
    headLength (Head (HeadRow lst)) = length lst

-- parse a first row -> list
-- parse a separator (at least 3 '-' in each section) (right length)
-- parse many filler rows (right length)

-- PADDING
-- before the block is MANY number of white spaces of any kind
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
pBlock = pTable <|> pHeading <|> pCode <|> pQuote <|> pFootnoteReference <|> pImage <|> pFreeText

-- MARKDOWN
markdownParser :: Parser Html
markdownParser = Html "title" <$> (Md <$> many pBlock)

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: Html -> String
convertADTHTML (Html t m) = header ++ prettyPrint delim m ++ footer
  where
    header = "<!DOCTYPE html>" ++ delim ++ "<html lang=\"en\">" ++ delim ++ delim ++ "<head>" ++ delim ++ "    <meta charset=\"UTF-8\">" ++ delim ++ "    <title>" ++ t ++ "</title>" ++ delim ++ "</head>" ++ delim ++ delim
    delim = "\n"
    footer = delim ++ delim ++ "</html>" ++ delim
