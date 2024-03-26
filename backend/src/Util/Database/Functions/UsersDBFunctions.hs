module Util.Database.Functions.UsersDBFunctions
  ( selectAllFromUsersWhereAppDB,
    selectAllFromUsersAppDB,
    insertAllIntoUsersAppDB,
    updateInUsersWhereAppDB,
    updateInUsersAppDB,
    selectFromUsersWhereAppDB,
    selectFromUsersAppDB,
    deleteFromUsersWhereAppDB,
    deleteFromUsersAppDB,
    updateAllInUsersAppDB,
    updateAllInUsersWhereAppDB,
  )
where

import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.ToField (ToField)
import Models.DBUser (DBUser)
import Util.Database.DBFunctions (deleteFromTableAppDB, deleteFromTableWhereAppDB, insertAllIntoTableAppDB, selectFromTableAppDB, selectFromTableWhereAppDB, updateInTableAppDB, updateInTableWhereAppDB)

selectFromUsersWhereAppDB :: (FromRow a, ToField b) => [String] -> [(String, String, b)] -> IO [a]
selectFromUsersWhereAppDB = selectFromTableWhereAppDB "users"

selectFromUsersAppDB :: (FromRow a) => [String] -> IO [a]
selectFromUsersAppDB = selectFromTableAppDB "users"

selectAllFromUsersWhereAppDB :: (ToField b) => [(String, String, b)] -> IO [DBUser]
selectAllFromUsersWhereAppDB = selectFromTableWhereAppDB "users" ["*"]

selectAllFromUsersAppDB :: IO [DBUser]
selectAllFromUsersAppDB = selectFromTableAppDB "users" ["*"]

insertAllIntoUsersAppDB :: (ToField a) => [a] -> IO ()
insertAllIntoUsersAppDB = insertAllIntoTableAppDB "users"

updateAllInUsersAppDB :: [String] -> IO ()
updateAllInUsersAppDB newValues = updateInUsersAppDB $ zip ["name", "email", "password", "type", "enrollment_number", "university_name"] newValues

updateAllInUsersWhereAppDB :: [String] -> [(String, String, String)] -> IO ()
updateAllInUsersWhereAppDB newValues = updateInUsersWhereAppDB $ zip ["name", "email", "password", "type", "enrollment_number", "university_name"] newValues

updateInUsersAppDB :: [(String, String)] -> IO ()
updateInUsersAppDB = updateInTableAppDB "users"

updateInUsersWhereAppDB :: [(String, String)] -> [(String, String, String)] -> IO ()
updateInUsersWhereAppDB = updateInTableWhereAppDB "users"

deleteFromUsersAppDB :: IO ()
deleteFromUsersAppDB = deleteFromTableAppDB "users"

deleteFromUsersWhereAppDB :: (ToField b) => [(String, String, b)] -> IO ()
deleteFromUsersWhereAppDB = deleteFromTableWhereAppDB "users"