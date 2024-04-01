{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Models.DB.DBWarningNotification (DBWarningNotification (..)) where

import Data.Aeson.Types (FromJSON, ToJSON)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import GHC.Generics (Generic)

data DBWarningNotification = DBWarningNotification
  { dbWarningId :: String,
    dbWarnedUserId :: Int
  }
  deriving (Show, Read, Eq, Generic)

instance FromRow DBWarningNotification

instance ToRow DBWarningNotification

instance ToJSON DBWarningNotification

instance FromJSON DBWarningNotification