{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Models.WarningNotification (WarningNotification (..)) where

import Data.Aeson.Types (FromJSON, ToJSON)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), RowParser, field)
import Database.PostgreSQL.Simple.ToField (Action, ToField (toField))
import Database.PostgreSQL.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import Models.User (User)

data WarningNotification = WarningNotification
  { warningId :: String,
    warnedUser :: User
  }
  deriving (Show, Read, Eq, Generic)

instance FromRow WarningNotification where
  fromRow :: RowParser WarningNotification
  fromRow = WarningNotification <$> field <*> fromRow

instance ToRow WarningNotification where
  toRow :: WarningNotification -> [Action]
  toRow (WarningNotification warningId' warnedUser') =
    toField warningId' : toRow warnedUser'

instance ToJSON WarningNotification

instance FromJSON WarningNotification