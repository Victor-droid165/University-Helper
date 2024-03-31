{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Models.Notebook
  ( Notebook (..),
    fromDBNotebook,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock ()
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), RowParser, field)
import Database.PostgreSQL.Simple.ToField (Action (Plain), ToField (toField))
import Database.PostgreSQL.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import Lib (stringToData, writeDataOnFile)
import Models.DBNotebook (DBNotebook (..))

data NotebookType = Reminder | StickyNotebook | PlainText | Warning deriving (Show, Read, Eq)

data Visibility = Private | Public deriving (Show, Read, Eq)

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
