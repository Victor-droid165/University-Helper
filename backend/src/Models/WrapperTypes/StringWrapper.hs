{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Models.WrapperTypes.StringWrapper
  ( StringWrapper (..),
    extractString,
  )
where

import Data.Aeson (FromJSON)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import GHC.Generics (Generic)

newtype StringWrapper = StringWrapper String deriving (Generic, FromJSON)

instance FromRow StringWrapper

instance ToRow StringWrapper

extractString :: StringWrapper -> String
extractString (StringWrapper n) = n