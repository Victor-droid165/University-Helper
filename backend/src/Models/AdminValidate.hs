{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE NamedFieldPuns #-}

module Models.AdminValidate (AdminV (..)) where

import Data.Aeson.Types (FromJSON, ToJSON)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow))
import Database.PostgreSQL.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import Data.Time (LocalTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM, formatTime, iso8601DateFormat)

data AdminV = AdminV
  { adminId :: Integer,
    userId :: Integer,
    timeStamp :: LocalTime
  } deriving (Show, Read, Eq, Generic)


instance FromRow AdminV

instance ToRow AdminV

instance ToJSON AdminV

instance FromJSON AdminV

