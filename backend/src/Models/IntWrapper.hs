{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Models.IntWrapper
  ( IntWrapper (..),
    extractInt,
  )
where

import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import GHC.Generics (Generic)

newtype IntWrapper = IntWrapper Int deriving (Eq, Ord, Show, Generic)

instance FromRow IntWrapper
instance ToRow IntWrapper

extractInt :: IntWrapper -> Int
extractInt (IntWrapper n) = n