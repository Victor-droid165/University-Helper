{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Models.DBUser (DBUser (..)) where

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
    validatorId :: Int,
    dbUserId :: Int
  }
  deriving (Show, Generic)

instance FromRow DBUser

instance ToRow DBUser