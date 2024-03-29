module Repositories.UserRepository
  ( getUsersFromDB,
    getUserFromDBWhere,
    removeUserFromDBByEnroll,
    removeUserFromDBByEmail,
    removeUserFromDBById,
    removeUserFromDB,
    updateUserInDB,
    createUserInDB,
    getDBusersFromDB,
    getUserField,
  )
where

import Models.User (User (..), fromDBUser)
import Util.Database.Functions.UsersDBFunctions (deleteFromUsersWhereAppDB, selectAllFromUsersAppDB, updateAllInUsersWhereAppDB, insertAllIntoUsersAppDB, selectAllFromUsersWhereAppDB, selectFromUsersWhereAppDB)
import Models.DBUser (DBUser)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple (FromRow)

getUsersFromDB :: IO [User]
getUsersFromDB = map fromDBUser <$> selectAllFromUsersAppDB

getDBusersFromDB :: IO [DBUser]
getDBusersFromDB = selectAllFromUsersAppDB

getUserFromDBWhere :: ToField b => [(String, String, b)] -> IO User
getUserFromDBWhere conditions = fromDBUser . head <$> selectAllFromUsersWhereAppDB conditions

getUserField :: (FromRow a) => User -> String -> IO a
getUserField user field = do 
  result <- selectFromUsersWhereAppDB [field] [("email", "=", userEmail user)]
  return $ head result

createUserInDB :: User -> IO ()
createUserInDB user = do
  let newUserValues = [userName user, userEmail user, userPassword user, userType user, userEnrollment user, userUniversity user]
  insertAllIntoUsersAppDB newUserValues

updateUserInDB :: User -> IO ()
updateUserInDB user = do
  let newValues = [userName user, userEmail user, userPassword user, userEnrollment user, userType user, userUniversity user]
  updateAllInUsersWhereAppDB newValues [("enrollment_number", "=", userEnrollment user)]

removeUserFromDB :: User -> IO ()
removeUserFromDB = removeUserFromDBByEmail . userEmail

removeUserFromDBByEnroll :: String -> IO ()
removeUserFromDBByEnroll enroll = deleteFromUsersWhereAppDB [("enrollment_number", "=", enroll)]

removeUserFromDBByEmail :: String -> IO ()
removeUserFromDBByEmail email = deleteFromUsersWhereAppDB [("email", "=", email)]

removeUserFromDBById :: Int -> IO ()
removeUserFromDBById userId = deleteFromUsersWhereAppDB [("id", "=", userId)]