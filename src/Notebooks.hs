module Notebooks where

import qualified Data.HashMap.Strict as HashMap
import Notes

type Page = [Note]

data Subject = Subject
  { subjectName :: String,
    subjectPages :: [Page]
  }
  deriving (Show)

data NotebookCommonInfo = NotebookCommonInfo
  { name :: String,
    notes :: [Note]
  }
  deriving (Show)

data Notebook
  = Normal NormalNotebook
  | Mental MentalNotebook
  deriving (Show)

data NormalNotebook = Conventional ConventionalNotebook | Chronological ChronologicalNotebook deriving (Show)

data ConventionalNotebook = ConventionalNotebook
  { cnInfo :: NotebookCommonInfo,
    pagesCount :: Int,
    schoolSubjects :: [Subject],
    cnMaxPageLength :: Int
  }
  deriving (Show)

data ChronologicalNotebook = ChronologicalNotebook
  { chnInfo :: NotebookCommonInfo,
    pages :: Maybe [Page],
    chnMaxPageLength :: Maybe Int
  }
  deriving (Show)

data MentalNotebook = MentalNotebook
  { mnInfo :: NotebookCommonInfo,
    keywordMap :: HashMap.HashMap String (Either NormalNotebook Note)
  }
  deriving (Show)
