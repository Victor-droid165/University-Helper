module Notebooks
  ( Notebook (..),
    NormalNotebook (..),
    MentalNotebook (..),
    NotebookFields (..),
    createNotebook,
    NotebookCommonInfo (..)
  )
where

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
  | Special MentalNotebook
  deriving (Show)

data NormalNotebook
  = ConventionalNotebook
      { cnInfo :: NotebookCommonInfo,
        pagesCount :: Int,
        schoolSubjects :: [Subject],
        cnMaxPageLength :: Int
      }
  | ChronologicalNotebook
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

data NotebookFields
  = ConventionalNotebookFields NotebookCommonInfo Int [Subject] Int
  | ChronologicalNotebookFields NotebookCommonInfo (Maybe [Page]) (Maybe Int)
  | MentalNotebookFields NotebookCommonInfo
  deriving (Show)


createNotebook :: NotebookFields -> Notebook
createNotebook (ConventionalNotebookFields info pagesCount' schoolSubjects' cnMaxPageLength') =
  Normal $ ConventionalNotebook info pagesCount' schoolSubjects' cnMaxPageLength'
createNotebook (ChronologicalNotebookFields info pages' chnMaxPageLength') =
  Normal $ ChronologicalNotebook info pages' chnMaxPageLength'
createNotebook (MentalNotebookFields info) =
  Special $ MentalNotebook info HashMap.empty