{-# LANGUAGE DeriveGeneric #-}

module Models.Notebook
  ( Notebook (..),
    fromDBNotebook,
    notebookToString,
    stringToNotebook,
    writeNotebookOnFile,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock ()
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)
import Lib (stringToData, writeDataOnFile)
import Models.DB.DBNotebook (DBNotebook (..))

data Notebook = Notebook
  { notebookId :: String,
    notebookType :: String,
    notebookName :: String
  }
  deriving (Show, Read, Eq, Generic)

instance FromRow Notebook

instance ToRow Notebook

instance ToJSON Notebook

instance FromJSON Notebook

notebookToString :: Notebook -> String
notebookToString = show

stringToNotebook :: String -> Maybe Notebook
stringToNotebook = stringToData

writeNotebookOnFile :: FilePath -> Notebook -> IO ()
writeNotebookOnFile = writeDataOnFile

fromDBNotebook :: DBNotebook -> IO Notebook
fromDBNotebook dbNotebook = do
  return
    Notebook
      { notebookId = dbNotebookId dbNotebook,
        notebookType = dbNotebookType dbNotebook,
        notebookName = dbNotebookName dbNotebook
      }
