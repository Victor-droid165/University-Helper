{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Models.DBNotebook (DBNotebook (..), DBNotebookOnlyId (..)) where

import Data.Aeson.Types (FromJSON, ToJSON)

import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import GHC.Generics (Generic)

data DBNotebook = DBNotebook
  { dbNotebookId :: String,
    dbNotebookType :: String,
    dbNotebookName :: String
  }
  deriving (Show, Read, Eq, Generic)

instance FromRow DBNotebook

instance ToRow DBNotebook

instance ToJSON DBNotebook

instance FromJSON DBNotebook

newtype DBNotebookOnlyId = DBNotebookOnlyId
  { dbNotebookOnlyId :: Int
  }
  deriving (Show, Generic)

instance FromRow DBNotebookOnlyId

instance ToRow DBNotebookOnlyId
