{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Models.AdminValidate (AdminV (..)) where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Time (LocalTime)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import GHC.Generics (Generic)

data AdminV = AdminV
  { adminId :: Integer,
    userId :: Integer,
    timeStamp :: LocalTime
  }
  deriving (Show, Read, Eq, Generic)

instance FromRow AdminV

instance ToRow AdminV

instance ToJSON AdminV

instance FromJSON AdminV
