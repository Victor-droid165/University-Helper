{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Util.Server.Users.APIDatas
  ( LogInfo (..),
    RegisterInfo (..),
    ChangeData (..),
    RandomData (..),
  )
where

import Data.Aeson (FromJSON)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)
import Models.User (User (..))

data LogInfo = LogInfo {email :: String, password :: String} deriving (Eq, Show, Generic, FromJSON)

data RegisterInfo = RegisterInfo {u_type :: String, user :: User} deriving (Eq, Show, Generic, FromJSON)

data ChangeData = ChangeData {field :: String, newValue :: String, match :: String, matchValue :: String} deriving (Eq, Show, Generic, FromJSON)

newtype RandomData = RandomData {userType' :: String} deriving (Show, Read, Eq, Generic)

instance FromRow RandomData

instance ToRow RandomData