module Exam.Type
  where

import Data.Text

-- * Types
data Exam = Exam { year      :: Text,
                   edition   :: Text,
                   questions :: [Question]
                 } deriving (Eq,Show)

data Question = Question { valid  :: Bool,
                           number :: Text,
                           area   :: Area,
                           instr   :: Instr,
                           items  :: [Item]
                         } deriving (Eq,Show)

data Item = Item { letter  :: Letter,
                   correct :: Bool,
                   text    :: Text
                 } deriving (Eq,Show)

type Letter = Text
type Area = [Text]
type Instr = Text
