{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Models.DBUser (DBUser (..), DBUserOnlyId (..)) where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Time (LocalTime)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import GHC.Generics (Generic)

data DBUser = DBUser
  { dbUserName :: String,
    dbUserEmail :: String,
    dbUserPassword :: String,
    dbUserType :: String,
    dbUserEnrollment :: String,
    dbUserUniversity :: String,
    dbIsDeleted :: Bool,
    dbUsercreatedAt :: LocalTime,
    dbUserId :: Int
  }
  deriving (Show, Read, Eq, Generic)

instance FromRow DBUser

instance ToRow DBUser

instance ToJSON DBUser

instance FromJSON DBUser

newtype DBUserOnlyId = DBUserOnlyId
  { dbUserOnlyId :: Int
  }
  deriving (Show, Generic)

instance FromRow DBUserOnlyId

instance ToRow DBUserOnlyId
