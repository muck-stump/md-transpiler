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

stringToBool :: String -> Bool
stringToBool "true" = True
stringToBool _ = False

-- Magic code to convert key, value pairs to JSON to send back to the server
jsonResponse :: [(String, String)] -> ActionM ()
jsonResponse pairs =
  json $ object [fromString key .= ((pack value) :: Text) | (key, value) <- pairs]

-- writeFile "the path" "the contents"
-- getTime >>= (\s -> writeFile (s ++ ".html") "The content")

makeTitle :: String -> IO String
makeTitle t = do
  time <- getTime
  return (t ++ "_" ++ time ++ ".html")

safeTitle :: String -> String
safeTitle "" = "Test"
safeTitle t = t

main :: IO ()
main = scotty 3000 $ do
  post "/api/convertMD" $ do
    requestBody <- body
    -- Convert the raw request body from ByteString to Text
    let requestBodyText = decodeUtf8 requestBody
        -- Convert the Text to String
        str = lines $ unpack requestBodyText
        save = stringToBool $ head str
        title = safeTitle $ str !! 1
        content = unlines $ drop 2 str
        -- markdownParser gets run with content
        -- Parse the Markdown string using 'markdownParser' and apply 'convertAllHTML'
        converted_html = getResult (parse markdownParser content) (convertADTHTML title)
    when save $ liftIO $ makeTitle title >>= (`writeFile` converted_html)

    -- Respond with the converted HTML as JSON
    jsonResponse [("html", converted_html)]
