module PrettyBoy where

import Control.Applicative (liftA3)
import Data.List (intercalate)
import Types
  ( ADT (..),
    AltText (AltText),
    Block (..),
    Body (..),
    CaptionText (CaptionText),
    Cell (..),
    CodeLine (..),
    Content (..),
    Head (..),
    HeadCell (..),
    HeadRow (..),
    ImgURL (ImgURL),
    Language (Language),
    Line (..),
    ModText (..),
    Program (Program),
    Row (..),
    Title (..),
  )

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

instance PrettyBoy ADT where
  preText :: String -> ADT -> String
  preText delim (ADT (Title t) _) = "<!DOCTYPE html>" ++ delim ++ "<html lang=\"en\">" ++ delim ++ delim ++ "<head>" ++ delim ++ "    <meta charset=\"UTF-8\">" ++ delim ++ "    <title>" ++ t ++ "</title>" ++ delim ++ "</head>" ++ delim ++ delim

  postText :: String -> ADT -> String
  postText delim _ = delim ++ "</html>" ++ delim

  unwrapPrint :: String -> ADT -> String
  unwrapPrint d (ADT _ c) = prettyPrint d c ++ d

instance PrettyBoy Content where
  preText :: String -> Content -> String
  preText delim _ = "<body>" ++ delim ++ "    "

  postText :: String -> Content -> String
  postText delim _ = "</body>"

  unwrapPrint :: String -> Content -> String
  unwrapPrint d (Content a) = prettyPrintAux (d ++ "    ") a ++ d

instance PrettyBoy Block where
  preText :: String -> Block -> String
  preText _ Image {} = "<p><img "
  preText _ (FootnoteReference n _) = "<p id=\"fn" ++ show n ++ "\">"
  preText delim (Quote _) = "<blockquote>" ++ delim ++ "    "
  preText _ (Code (Language "") _) = "<pre><code>"
  preText _ (Code (Language lang) _) = "<pre><code class=\"language-" ++ lang ++ "\">"
  preText _ (Heading depth _) = "<h" ++ show depth ++ ">"
  preText _ (Table {}) = "<table>"
  preText _ _ = "<p>"

  postText :: String -> Block -> String
  postText _ Image {} = "></p>"
  postText _ (Quote _) = "</blockquote>"
  postText _ (Code {}) = "</code></pre>"
  postText _ (Heading depth _) = "</h" ++ show depth ++ ">"
  postText _ (Table {}) = "</table>"
  postText _ _ = "</p>"

  unwrapPrint :: String -> Block -> String
  unwrapPrint d (Quote a) = prettyPrintAux sep a ++ d
    where
      sep = d ++ "    "
  unwrapPrint d (Table hd bd) =
    sep
      ++ prettyPrint sep hd
      ++ sep
      ++ prettyPrint sep bd
      ++ d
    where
      sep = d ++ "    "
  unwrapPrint _ (FootnoteReference _ txt) = txt
  unwrapPrint _ (Image (AltText alt) (ImgURL url) (CaptionText txt)) = "src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\" title=\"" ++ txt ++ "\""
  unwrapPrint _ (FreeText a) = prettyPrintAux "" a
  unwrapPrint _ (Code _ (Program prog)) = prog
  unwrapPrint _ (Heading _ text) = prettyPrintAux "" text

-- unwrapPrint _ _ = undefined

-- unwrapPrint d (FreeText a) = sep ++ prettyPrintAux "" a ++ d
--   where
--     sep = d ++ "    "

instance PrettyBoy Cell where
  preText :: String -> Cell -> String
  preText _ _ = "<td>"
  postText :: String -> Cell -> String
  postText _ _ = "</td>"
  unwrapPrint :: String -> Cell -> String
  unwrapPrint _ (Cell a) = prettyPrintAux "" a

instance PrettyBoy HeadCell where
  preText :: String -> HeadCell -> String
  preText _ _ = "<th>"
  postText :: String -> HeadCell -> String
  postText _ _ = "</th>"
  unwrapPrint :: String -> HeadCell -> String
  unwrapPrint _ (HeadCell a) = prettyPrintAux "" a

instance PrettyBoy Head where
  preText :: String -> Head -> String
  preText _ _ = "<thead>"
  postText :: String -> Head -> String
  postText _ _ = "</thead>"
  unwrapPrint :: String -> Head -> String
  unwrapPrint d (Head a) = sep ++ prettyPrint sep a ++ d
    where
      sep = d ++ "    "

instance PrettyBoy HeadRow where
  preText :: String -> HeadRow -> String
  preText _ _ = "<tr>"
  postText :: String -> HeadRow -> String
  postText _ _ = "</tr>"
  unwrapPrint :: String -> HeadRow -> String
  unwrapPrint d (HeadRow a) = sep ++ prettyPrintAux sep a ++ d
    where
      sep = d ++ "    "

instance PrettyBoy Body where
  preText :: String -> Body -> String
  preText _ _ = "<tbody>"

  postText :: String -> Body -> String
  postText _ _ = "</tbody>"

  unwrapPrint :: String -> Body -> String
  unwrapPrint d (Body rows) = sep ++ prettyPrintAux sep rows ++ d
    where
      sep = d ++ "    "

instance PrettyBoy Row where
  preText :: String -> Row -> String
  preText _ _ = "<tr>"
  postText :: String -> Row -> String
  postText _ _ = "</tr>"
  unwrapPrint :: String -> Row -> String
  unwrapPrint d (Row a) = sep ++ prettyPrintAux sep a ++ d
    where
      sep = d ++ "    "

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
  preText _ (Super _) = "<sup>"
  preText _ (Sub _) = "<sub>"

  postText :: String -> ModText -> String
  postText _ (Bold _) = "</strong>"
  postText _ (Italic _) = "</em>"
  postText _ (StrikeThrough _) = "</del>"
  postText _ (InlineCode _) = "</code>"
  postText _ (Untouched _) = ""
  postText _ (Link _) = "</a>"
  postText _ (Footnote _) = "</sup>"
  postText _ (Super _) = "</sup>"
  postText _ (Sub _) = "</sub>"

  unwrapPrint :: String -> ModText -> String
  unwrapPrint _ (Bold a) = prettyPrintAux "" a
  unwrapPrint _ (Italic a) = prettyPrintAux "" a
  unwrapPrint _ (StrikeThrough a) = prettyPrintAux "" a
  unwrapPrint _ (Super a) = prettyPrintAux "" a
  unwrapPrint _ (Sub a) = prettyPrintAux "" a
  unwrapPrint _ (InlineCode a) = a
  unwrapPrint _ (Untouched a) = [a]
  unwrapPrint _ (Link a) = snd a ++ "\">" ++ fst a
  unwrapPrint _ (Footnote a) = "<a id=\"fn" ++ n ++ "ref\" href=\"#fn" ++ n ++ "\">" ++ n ++ "</a>"
    where
      n = show a

-- "<!DOCTYPE html>" ++ delim ++ "<html lang=\"en\">" ++ delim ++ delim ++ "<head>" ++ delim ++ "    <meta charset=\"UTF-8\">" ++ delim ++ "    <title>Test</title>" ++ delim ++ "</head>" ++ delim ++ delim
-- "<body>" ++ delim ++ "    "
-- ++ delim ++ delim ++ "</html>" ++ delim