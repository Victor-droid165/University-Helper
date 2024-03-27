module Repositories.UserRepository
  ( getUsersFromDB,
    getUserFromDB,
    removeUserFromDBByEnroll,
    removeUserFromDBByEmail,
    removeUserFromDBById,
    removeUserFromDB,
    updateUserInDB,
    createUserInDB,
    getDBusersFromDB,
  )
where

import Models.User (User (..), fromDBUser)
import Util.Database.Functions.UsersDBFunctions (deleteFromUsersWhereAppDB, selectAllFromUsersAppDB, updateAllInUsersWhereAppDB, insertAllIntoUsersAppDB)
import Models.DBUser (DBUser)

getUsersFromDB :: IO [User]
getUsersFromDB = map fromDBUser <$> selectAllFromUsersAppDB

getDBusersFromDB :: IO [DBUser]
getDBusersFromDB = selectAllFromUsersAppDB

getUserFromDB :: IO User
getUserFromDB = fromDBUser . head <$> selectAllFromUsersAppDB

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