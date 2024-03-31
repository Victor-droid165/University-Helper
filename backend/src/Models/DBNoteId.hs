{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Models.DBNoteId (DBNoteId (..)) where

import Data.Aeson.Types (FromJSON, ToJSON)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import GHC.Generics (Generic)

data DBNoteId = DBNoteId
  { dbPrefix :: String,
    dbIdNum :: Int
  }
  deriving (Show, Read, Eq, Generic)

instance FromRow DBNoteId

instance ToRow DBNoteId

instance ToJSON DBNoteId

instance FromJSON DBNoteId