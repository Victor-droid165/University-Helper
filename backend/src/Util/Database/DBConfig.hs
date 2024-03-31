{-# LANGUAGE OverloadedStrings #-}

module Util.Database.DBConfig (
  DBConfig(..)
) where

import Data.Yaml (FromJSON(..), (.:), withObject)

data DBConfig = DBConfig
  { dbHost :: String
  , dbPort :: Int
  , dbName :: String
  , dbUser :: String
  , dbPassword :: String
  } deriving (Show)

instance FromJSON DBConfig where
  parseJSON = withObject "DBConfig" $ \obj -> do
    host <- obj .: "db_host"
    port <- obj .: "db_port"
    name <- obj .: "db_name"
    user <- obj .: "db_user"
    password <- obj .: "db_password"
    return $ DBConfig host port name user password