-- | Implement a limited JSON parser.
--
-- Use the provided JsonValue type to create a parser for JSON values.
-- A good starting point is to consider how to parse the literal types
-- such as JTrue, JFalse, JNull, JInteger, and JString.
--
-- Make sure to refer to Instances.hs and Parser.hs for utility functions,
-- parsers, and parser combinators to help you.
--
-- **Reminders**
-- You must NOT manually construct the Parser type using the Parser constructor,
-- nor use the `parse` function to chain together parsers. You must also NOT
-- use the `reads` function or equivalent. The cases where this is the
-- intended solution have already been provided to you.
--
-- The README contains a list of useful functions and operators worth looking at.
--
-- see https://tgdwyer.github.io/parsercombinators/#a-parser-that-returns-an-adt
module JSON (thisJson, getKey, getString, getBool, sepBy, JsonValue (JNull), tok, stringTok) where

import Control.Applicative
  ( Alternative (many, some, (<|>)),
    Applicative (liftA2),
  )
import Data.Foldable (asum)
import Instances (Parser (..), parse)
import Parser (int, is, isNot, space, spaces, string)

type KeyVal = (String, JsonValue)

-- | JSON value representation.
data JsonValue
  = JInteger Int
  | JString String
  | JObject [KeyVal]
  | JArray [JsonValue]
  | JTrue
  | JFalse
  | JNull
  deriving (Show, Eq)

-- | Write a function that applies the given parser, then
--  parse a space character if it exists.
--
-- >>> parse (tok (is 'a')) "a bc"
-- Just ("bc",'a')
--
-- >>> parse (tok (is 'a')) "abc"
-- Just ("bc",'a')
tok :: Parser a -> Parser a
-- tok a = a <* is ' '
-- tok = flip (<*) spaces
tok = (<* spaces)

-- const <$> (C a) <*> (C b)
-- (C const a b)
-- (C a)
-- f <$> (C a) <*> (C b)
-- (C f a) <*> (C b)
-- (C f b)

-- (+ 1)
--

-- | Write a function that parses the given char followed by 0 or more spaces.
--
-- /Hint/: Remember the `is` parser
--
-- >>> parse (isTok 'a') "abc"
-- Just ("bc",'a')
--
-- >>> parse (isTok 'a') "dabc"
-- Nothing
isTok :: Char -> Parser Char
isTok = tok . is

-- | Write a function that parses a comma followed by 0 or more spaces.
--
-- /Hint/: Remember the `is` parser
--
-- >>> parse commaTok ", bc"
-- Just ("bc",',')
--
-- >>> parse commaTok "dabc"
-- Nothing
commaTok :: Parser Char
commaTok = isTok ','

-- | Write a function that parses the given string, followed by 0 or more
-- spaces.
--
-- /Hint/: Remember the `string` parser
--
-- >>> parse (stringTok "abc") "abc  "
-- Just ("","abc")
--
-- >>> parse (stringTok "abc") "bc  "
-- Nothing
stringTok :: String -> Parser String
stringTok = tok . string

-- | Parse a JSON integer.
--
-- >>> parse jsonInteger "234"
-- Just ("",JInteger 234)
--
-- >>> parse jsonInteger "-234"
-- Just ("",JInteger (-234))
--
-- >>> parse jsonInteger "-123.45"
-- Nothing
--
-- >>> parse jsonInteger "-"
-- Nothing
--
-- >>> parse jsonInteger "abc"
-- Nothing
jsonInteger :: Parser JsonValue
jsonInteger = JInteger <$> tok int

-- | Parse a JSON true literal.
-- /Hint/ Useful function PCQ=
--
-- >>> parse jsonTrue "true"
-- Just ("",JTrue)
--
-- >>> parse jsonTrue "TRUE"
-- Nothing
jsonTrue :: Parser JsonValue
jsonTrue = JTrue <$ stringTok "true"

-- | Parse a JSON false literal.
--
-- >>> parse jsonFalse "false"
-- Just ("",JFalse)
--
-- >>> parse jsonFalse "FALSE"
-- Nothing
jsonFalse :: Parser JsonValue
jsonFalse = JFalse <$ stringTok "false"

-- | Parse a JSON false boolean.
--
-- >>> parse jsonBool "false"
-- Just ("",JFalse)
--
-- >>> parse jsonBool "true"
-- Just ("",JTrue)
--
-- >>> parse jsonBool "TRUE"
-- Nothing
jsonBool :: Parser JsonValue
jsonBool = jsonTrue <|> jsonFalse

-- | Parse a JSON null literal.
--
-- >>> parse jsonNull "null"
-- Just ("",JNull)
--
-- >>> parse jsonNull "NULL"
-- Nothing
jsonNull :: Parser JsonValue
jsonNull = JNull <$ string "null"

-- asd

-- | Parse a sequence of at least one values with a separator.
--
-- >>> parse ((isTok 'a') `sepBy1` commaTok) ""
-- Nothing
--
-- >>> parse ((isTok 'a') `sepBy1` commaTok) "a"
-- Just ("","a")
--
-- >>> parse ((isTok 'a') `sepBy1` commaTok) "a,a"
-- Just ("","aa")
--
-- >>> parse ((tok int) `sepBy1` commaTok) "1,2,3"
-- Just ("",[1,2,3])
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 a b = liftA2 (:) a $ many (b *> a)

-- sepBy1 a b = (:) <$> a <*> (many (b *> a))

-- i need to fmap here somewhere to get things consing ??
-- seems like it doesn't do nothing when I get then null
-- where
--   -- (Optional) cons the results of two parsers
--   (<:>) :: Parser a -> Parser [a] -> Parser [a]
--   (<:>) = undefined

-- | Parse a sequence of values with a separator.
--
-- >>> parse ((isTok 'a') `sepBy` commaTok) ""
-- Just ("","")
--
-- >>> parse ((isTok 'a') `sepBy` commaTok) "a"
-- Just ("","a")
--
-- >>> parse ((isTok 'a') `sepBy` commaTok) "a,a"
-- Just ("","aa")
--
-- >>> parse ((tok int) `sepBy` commaTok) "1,2,3"
-- Just ("",[1,2,3])
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy a b = sepBy1 a b <|> pure []

-- sepBy = (many .) . (<*)

-- | A quoteString is any series of any non-" characters surrounded by " "
--
-- >>> parse quoteString "\" abc\""
-- Just (""," abc")
--
-- >>> parse quoteString "\"abc\"def"
-- Just ("def","abc")
--
-- >>> parse quoteString "abc"
-- Nothing
--
-- >>> parse quoteString "\"\\abc\"def"
-- Just ("def","\\abc")
quoteString :: Parser String
quoteString = is '\"' *> some (isNot '"') <* isTok '\"'

-- | Parse a JSON string. Handle double-quotes.
--
-- >>> parse jsonString "\" abc\""
-- Just ("",JString " abc")
--
-- >>> parse jsonString "\"abc\"def"
-- Just ("def",JString "abc")
--
-- >>> parse jsonString "abc"
-- Nothing
--
-- >>> parse jsonString "\"\\abc\"def"
-- Just ("def",JString "\\abc")
jsonString :: Parser JsonValue
jsonString = JString <$> quoteString

-- | Parse a JSON array.
--
-- /Hint/: Remember the type [JsonValue] means a list of JsonValues, and the
--  parser for JsonValue is called jsonValue.
--
-- /Hint 2/: This parser is co-recursive!
--  To test this parser, you will need to implement jsonValue as well.
--
-- see https://tgdwyer.github.io/parsercombinators/#creating-a-parse-tree
--
-- >>> parse jsonArray "[]"
-- Just ("",JArray [])
--
-- >>> parse jsonArray "[true]"
-- Just ("",JArray [JTrue])
--
-- >>> parse jsonArray "[true, 5, []]"
-- Just ("",JArray [JTrue,JInteger 5,JArray []])
jsonArray :: Parser JsonValue
jsonArray = JArray <$> parseArray
  where
    parseArray = isTok '[' *> thisJson `sepBy` commaTok <* isTok ']'

-- | Parse a JSON object.
--
-- /Hint/: Remember the type KeyVal = [(String, JsonValue)].
--
-- /Hint 2/: This parser is also co-recursive!
--  To test this parser, you will need to implement jsonValue as well.
--
-- >>> parse jsonObject "{}"
-- Just ("",JObject [])
--
-- >>> parse jsonObject "{ \"key1\" : true }"
-- Just ("",JObject [("key1",JTrue)])
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false }"
-- Just ("",JObject [("key1",JTrue),("key2",JFalse)])
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false } xyz"
-- Just ("xyz",JObject [("key1",JTrue),("key2",JFalse)])
jsonObject :: Parser JsonValue
jsonObject = JObject <$> (isTok '{' *> kv `sepBy` commaTok <* isTok '}')
  where
    kv :: Parser (String, JsonValue)
    kv = (,) <$> k <*> v
    k = tok quoteString <* isTok ':'
    v = tok thisJson

-- | Parse a JSON value
-- Either a Boolean, Integer, String, Null
--
-- >>> parse jsonVal "\" abc\""
-- Just ("",JString " abc")
--
-- >>> parse jsonVal "false"
-- Just ("",JFalse)
--
-- >>> parse jsonVal "234"
-- Just ("",JInteger 234)
--
-- >>> parse jsonVal "null"
-- Just ("",JNull)
jsonVal :: Parser JsonValue
jsonVal = jsonBool <|> jsonInteger <|> jsonString <|> jsonNull

-- | Parse a JSON container, either an array or an object.
--
-- >>> parse jsonContainer "[true, 5, []]"
-- Just ("",JArray [JTrue,JInteger 5,JArray []])
--
-- >>> parse jsonContainer "{ \"key1\" : true , \"key2\" : false } xyz"
-- Just ("xyz",JObject [("key1",JTrue),("key2",JFalse)])
jsonContainer :: Parser JsonValue
jsonContainer = jsonArray <|> jsonObject

-- | Parse a JSON string.
--
-- /Hint/: Use the parsers we have just made for each component of the JSON
--  type.
--
--
-- >>> parse thisJson "true"
-- Just ("",JTrue)
--
-- >>> parse thisJson "{ \"key1\" : true , \"key2\" : [7, false] }"
-- Just ("",JObject [("key1",JTrue),("key2",JArray [JInteger 7,JFalse])])
--
-- >>> parse thisJson "{ \"key1\" : true , \"key2\" : [7, false] , \"key3\" : { \"key4\" : null } }"
-- Just ("",JObject [("key1",JTrue),("key2",JArray [JInteger 7,JFalse]),("key3",JObject [("key4",JNull)])])
thisJson :: Parser JsonValue
thisJson = jsonVal <|> jsonContainer

-- FROM APPLIED
-- FROM APPLIED
-- FROM APPLIED

getString :: Maybe JsonValue -> String
getString (Just (JString s)) = s
getString _ = error "Expected a JString, but got something else"

getBool :: Maybe JsonValue -> Bool
getBool (Just JTrue) = True
getBool (Just JFalse) = False
getBool _ = False

-- | Get the value corresponding to a key in a JSON object.
--
-- >>> let json = JObject [("key1", JString "value1"), ("key2", JString "value2")]
-- >>> getKey "key1" json
-- Just (JString "value1")
-- >>> getKey "key3" json
-- Nothing
getKey :: String -> JsonValue -> Maybe JsonValue
getKey k (JObject kv) = lookup k kv
getKey _ _ = Nothing

-- | Wraps a function that extracts a value from a JSON value in a `Maybe`,
--   returning `Nothing` if the JSON value is `null`.
getOptional :: (Maybe JsonValue -> a) -> Maybe JsonValue -> Maybe a
getOptional _ (Just JNull) = Nothing
getOptional f x = Just $ f x