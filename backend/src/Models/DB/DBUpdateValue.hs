{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.DB.DBUpdateValue (DBUpdateValue (..)) where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data DBUpdateValue = DBUpdateValue
  { fieldToUpdate :: String,
    newValue :: String,
    whereField :: String,
    whereValue :: String
  }
  deriving (Eq, Show, Generic, FromJSON)