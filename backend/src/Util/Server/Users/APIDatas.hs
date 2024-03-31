{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Util.Server.Users.APIDatas
  ( IntegerData (..),
    MyData (..),
    LogInfo (..),
    RegisterInfo (..),
    ChangeData (..),
    RandomData (..),
    GetUserFieldData (..),
  )
where

import Data.Aeson (FromJSON)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)
import Models.User (User (..))

newtype IntegerData = IntegerData {integerValue :: Integer} deriving (Generic, FromJSON)

newtype MyData = MyData {value :: String} deriving (Generic, FromJSON)

data LogInfo = LogInfo {email :: String, password :: String} deriving (Eq, Show, Generic, FromJSON)

data RegisterInfo = RegisterInfo {u_type :: String, user :: User} deriving (Eq, Show, Generic, FromJSON)

data ChangeData = ChangeData {field :: String, newValue :: String, match :: String, matchValue :: String} deriving (Eq, Show, Generic, FromJSON)

newtype RandomData = RandomData {userType' :: String} deriving (Show, Read, Eq, Generic)

instance FromRow RandomData

instance ToRow RandomData

data GetUserFieldData = GetUserFieldData {unique_key_name :: Maybe String, unique_key :: Maybe String, attribute :: Maybe String} deriving (Eq, Show, Generic, FromJSON)