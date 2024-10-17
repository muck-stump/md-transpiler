{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Assignment (markdownParser)

import Assignment (convertADTHTML, getTime, markdownParser)
-- import qualified JSON as J

import Control.Monad.Cont (MonadIO (liftIO), when)
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Key (fromString)
import Data.Text.Lazy (Text, pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Instances (ParseResult (Result), parse)
import JSON
import Web.Scotty (ActionM, body, json, post, scotty)

getResult :: ParseResult a -> (a -> String) -> String
getResult (Result _ a) f = f a
getResult _ _ = ""

unwrapJSON :: ParseResult JsonValue -> JsonValue
unwrapJSON (Result _ a) = a
unwrapJSON _ = JNull

-- Magic code to convert key, value pairs to JSON to send back to the server
jsonResponse :: [(String, String)] -> ActionM ()
jsonResponse pairs =
  json $ object [fromString key .= ((pack value) :: Text) | (key, value) <- pairs]

-- writeFile "the path" "the contents"
-- getTime >>= (\s -> writeFile (s ++ ".html") "The content")

main :: IO ()
main = scotty 3000 $ do
  post "/api/convertMD" $ do
    requestBody <- body -- this is (originally) s.markdown in main.ts. I've set this to make all the state
    -- Convert the raw request body from ByteString to Text
    let requestBodyText = decodeUtf8 requestBody
        -- Convert the Text to String
        str = unpack requestBodyText
        wrappedObj = unwrapJSON (parse thisJson str)
        obj = getString $ getKey "content" wrappedObj
        save = getBool $ getKey "save" wrappedObj
        -- obj = decode requestBody :: Maybe (Map String String)
        -- decode str into save and content
        -- markdownParser gets run with content
        -- Parse the Markdown string using 'markdownParser' and apply 'convertAllHTML'
        converted_html = getResult (parse markdownParser obj) convertADTHTML
    when save $
      liftIO $
        getTime >>= (`writeFile` converted_html) . (++ ".html")

    -- Respond with the converted HTML as JSON
    jsonResponse [("html", converted_html)]
