module Util.Database.Functions.UsersDBFunctions
  ( selectAllFromUsersWhereAppDB,
  )
where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField (ToField)
import Util.Database.DBFunctions (selectFromTableWhereAppDB)

selectAllFromUsersWhereAppDB :: (FromRow a, ToField b) => [(String, String, b)] -> IO [a]
selectAllFromUsersWhereAppDB = selectFromTableWhereAppDB "users" ["*"]