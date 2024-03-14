{-# LANGUAGE OverloadedStrings #-}

module DBConfig
  ( DBConfig(..)
  , loadDBConfig
  ) where

import Data.Yaml (decodeFileThrow, FromJSON(..), (.:), withObject)

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

loadDBConfig :: FilePath -> IO (Maybe DBConfig)
loadDBConfig filePath = do
  config <- decodeFileThrow filePath
  return $ Just config
