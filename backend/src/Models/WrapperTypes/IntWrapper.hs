{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Models.WrapperTypes.IntWrapper (IntWrapper (..), extractInt) where

import Data.Aeson (FromJSON)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import GHC.Generics (Generic)

newtype IntWrapper = IntWrapper Int deriving (Generic, FromJSON)

instance FromRow IntWrapper

instance ToRow IntWrapper

extractInt :: IntWrapper -> Int
extractInt (IntWrapper n) = n