{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Models.DBNote (DBNote (..), DBNoteOnlyId (..)) where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Time (LocalTime)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import GHC.Generics (Generic)

data DBNote = DBNote
  { dbNoteId :: String,
    dbNoteType :: String,
    dbNoteVisibility :: String,
    dbNoteTitle :: Maybe String,
    dbNoteSubject :: Maybe String,
    dbNoteContent :: String,
    dbNoteCreatorId :: Int,
    dbNoteCreatedAt :: LocalTime,
    dbNoteUpdatedAt :: LocalTime
  }
  deriving (Show, Read, Eq, Generic)

instance FromRow DBNote

instance ToRow DBNote

instance ToJSON DBNote

instance FromJSON DBNote

newtype DBNoteOnlyId = DBNoteOnlyId
  { dbNoteOnlyId :: Int
  }
  deriving (Show, Generic)

instance FromRow DBNoteOnlyId

instance ToRow DBNoteOnlyId
