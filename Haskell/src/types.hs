module Types where

data ADT = ADT Title Content
  -- Your ADT **must** derive Show.
  deriving (Show, Eq)

data Content = Content [Block]
  deriving (Show, Eq)

data Title = Title String
  deriving (Show, Eq)

data Block
  = FreeText [ModText]
  | Image AltText ImgURL CaptionText
  | FootnoteReference Int String
  | Quote [Line]
  | Code Language Program
  | Heading Int [ModText]
  | Table Head Body
  deriving (Show, Eq)

data Head = Head HeadRow
  deriving (Show, Eq)

data HeadRow = HeadRow [HeadCell]
  deriving (Show, Eq)

data Body = Body [Row]
  deriving (Show, Eq)

data Row = Row [Cell]
  deriving (Show, Eq)

data Cell = Cell [ModText]
  deriving (Show, Eq)

data HeadCell = HeadCell [ModText]
  deriving (Show, Eq)

data Language = Language String
  deriving (Show, Eq)

data Program = Program String
  deriving (Show, Eq)

data CodeLine = CodeLine String
  deriving (Show, Eq)

data Line = Line [ModText]
  deriving (Show, Eq)

data ModText
  = Bold [ModText]
  | Italic [ModText]
  | StrikeThrough [ModText]
  | Super [ModText]
  | Sub [ModText]
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
