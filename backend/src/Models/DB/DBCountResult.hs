{-# LANGUAGE DeriveGeneric #-}

module Models.DB.DBCountResult (DBCountResult (..)) where

import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics (Generic)

newtype DBCountResult = DBCountResult Int deriving (Show, Read, Eq, Generic)

instance FromRow DBCountResult