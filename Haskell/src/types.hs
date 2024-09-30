module Types (Md, FreeText) where

-- This isn't being used
-- import Parser ()

data Md = Empty
  -- Your ADT **must** derive Show.
  deriving (Show, Eq)

data FreeText
  = Bold String
  | Italic String
  | StrikeThrough String
  | InlineCode String
  | Untouched String
  deriving (Show, Eq)