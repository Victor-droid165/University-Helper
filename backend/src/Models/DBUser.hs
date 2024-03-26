{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Models.DBUser (DBUser (..), DBUserOnlyId (..)) where

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
    dbUsercreatedAt :: LocalTime,
    dbUserId :: Int
  }
  deriving (Show, Generic)

instance FromRow DBUser

instance ToRow DBUser

newtype DBUserOnlyId = DBUserOnlyId
  { dbUserOnlyId :: Int
  }
  deriving (Show, Generic)

instance FromRow DBUserOnlyId

instance ToRow DBUserOnlyId
