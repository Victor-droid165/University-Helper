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

updateAllInUsersAppDB :: (ToField a) => [a] -> IO ()
updateAllInUsersAppDB newValues = updateInUsersAppDB $ zip ["name", "email", "password", "type", "enrollment_number", "university_name"] newValues

updateAllInUsersWhereAppDB :: (ToField a) => [a] -> [(String, String, a)] -> IO ()
updateAllInUsersWhereAppDB newValues = updateInUsersWhereAppDB $ zip ["name", "email", "password", "type", "enrollment_number", "university_name"] newValues 

updateInUsersAppDB :: (ToField a) => [(String, a)] -> IO ()
updateInUsersAppDB = updateInTableAppDB "users"

updateInUsersWhereAppDB :: (ToField b) => [(String, b)] -> [(String, String, b)] -> IO ()
updateInUsersWhereAppDB = updateInTableWhereAppDB "users"

deleteFromUsersAppDB :: IO ()
deleteFromUsersAppDB = deleteFromTableAppDB "users"

deleteFromUsersWhereAppDB :: (ToField b) => [(String, String, b)] -> IO ()
deleteFromUsersWhereAppDB = deleteFromTableWhereAppDB "users"