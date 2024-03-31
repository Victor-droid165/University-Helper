module Util.Database.Functions.ValidationDBFunctions
  ( selectAllFromValidationsWhereAppDB,
    selectAllFromValidationsAppDB,
    insertAllIntoValidationsAppDB,
    updateInValidationsWhereAppDB,
    updateInValidationsAppDB,
    selectFromValidationsWhereAppDB,
    selectFromValidationsAppDB,
    deleteFromValidationsWhereAppDB,
    deleteFromValidationsAppDB,
    updateAllInValidationsAppDB,
    updateAllInValidationsWhereAppDB,
  )
where

import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.ToField (ToField)
import Models.DB.DBUser (DBUser)
import Util.Database.DBFunctions (deleteFromTableAppDB, deleteFromTableWhereAppDB, insertAllIntoTableAppDB, selectFromTableAppDB, selectFromTableWhereAppDB, updateInTableAppDB, updateInTableWhereAppDB)

selectFromValidationsWhereAppDB :: (FromRow a, ToField b) => [String] -> [(String, String, b)] -> IO [a]
selectFromValidationsWhereAppDB = selectFromTableWhereAppDB "admin_user_validations"

selectFromValidationsAppDB :: (FromRow a) => [String] -> IO [a]
selectFromValidationsAppDB = selectFromTableAppDB "admin_user_validations"

selectAllFromValidationsWhereAppDB :: (ToField b) => [(String, String, b)] -> IO [DBUser]
selectAllFromValidationsWhereAppDB = selectFromTableWhereAppDB "admin_user_validations" ["*"]

selectAllFromValidationsAppDB :: (FromRow a) => IO [a]
selectAllFromValidationsAppDB = selectFromTableAppDB "admin_user_validations" ["*"]

insertAllIntoValidationsAppDB :: (ToField a) => [a] -> IO ()
insertAllIntoValidationsAppDB = insertAllIntoTableAppDB "admin_user_validations"

updateAllInValidationsAppDB :: [String] -> IO ()
updateAllInValidationsAppDB newValues = updateInValidationsAppDB $ zip ["admin_id", "user_id"] newValues

updateAllInValidationsWhereAppDB :: [String] -> [(String, String, String)] -> IO ()
updateAllInValidationsWhereAppDB newValues = updateInValidationsWhereAppDB $ zip ["admin_id", "user_id"] newValues

updateInValidationsAppDB :: [(String, String)] -> IO ()
updateInValidationsAppDB = updateInTableAppDB "admin_user_validations"

updateInValidationsWhereAppDB :: [(String, String)] -> [(String, String, String)] -> IO ()
updateInValidationsWhereAppDB = updateInTableWhereAppDB "admin_user_validations"

deleteFromValidationsAppDB :: IO ()
deleteFromValidationsAppDB = deleteFromTableAppDB "admin_user_validations"

deleteFromValidationsWhereAppDB :: (ToField b) => [(String, String, b)] -> IO ()
deleteFromValidationsWhereAppDB = deleteFromTableWhereAppDB "admin_user_validations"