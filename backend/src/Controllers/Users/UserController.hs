{-# LANGUAGE BinaryLiterals #-}

module Controllers.Users.UserController
  ( getUsers,
    swapUser,
    verifyLoginIO,
    findUserByEmail,
    findUserByEnroll,
    registerUserAPI,
    getDBUsers,
    registerStudentAPI,
    getUserById,
    getUserByEmail,
    removeUserById,
    removeUserByEnroll,
    updateUser,
    authenticateUser,
    filterByUserEnroll,
  )
where

import Data.Foldable (find)
import Lib (handleMaybe)
import Models.DB.DBUser (DBUser)
import Models.User
  ( User (..),
  )
import Models.WrapperTypes.IntWrapper (IntWrapper (..), extractInt)
import Repositories.UserRepository
  ( createUserInDB,
    getDBusersFromDB,
    getUserField,
    getUserFromDBWhere,
    getUsersFromDB,
    removeUserFromDBByEnroll,
    removeUserFromDBById,
    updateUserInDB,
  )
import Util.Database.Functions.ValidationDBFunctions (insertAllIntoValidationsAppDB)

getUserById :: Int -> IO User
getUserById userId = getUserFromDBWhere [("id", "=", userId)]

getUserByEmail :: String -> IO User
getUserByEmail userEmail' = getUserFromDBWhere [("email", "=", userEmail')]

getUsers :: IO [User]
getUsers = getUsersFromDB

getDBUsers :: IO [DBUser]
getDBUsers = getDBusersFromDB

updateUser :: User -> IO ()
updateUser = updateUserInDB

removeUserByEnroll :: String -> IO ()
removeUserByEnroll = removeUserFromDBByEnroll

removeUserById :: Int -> IO ()
removeUserById = removeUserFromDBById

authenticateUser :: String -> String -> IO (Maybe User)
authenticateUser email password =
  find (\user -> userEmail user == email && userPassword user == password) <$> getUsers

verifyLoginIO :: String -> String -> IO Bool
verifyLoginIO email password = do
  allUsers <- getUsers
  let user = findUserByEmail email allUsers
      userPassword' = handleMaybe user "" userPassword
  tryToPerformLogin userPassword'
  where
    tryToPerformLogin :: String -> IO Bool
    tryToPerformLogin userPassword'
      | userPassword' == password = return True
      | otherwise = return False

findUserByEmail :: String -> [User] -> Maybe User
findUserByEmail _ [] = Nothing
findUserByEmail email (user : users)
  | email == userEmail user = Just user
  | otherwise = findUserByEmail email users

findUserByEnroll :: String -> [User] -> Maybe User
findUserByEnroll _ [] = Nothing
findUserByEnroll enroll (user : users)
  | enroll == userEnrollment user = Just user
  | otherwise = findUserByEnroll enroll users

filterByUserEnroll :: String -> [User] -> [User]
filterByUserEnroll _ [] = []
filterByUserEnroll enroll (u : userList)
  | enroll == userEnrollment u = filterByUserEnroll enroll userList
  | otherwise = u : filterByUserEnroll enroll userList

registerUserAPI :: User -> IO ()
registerUserAPI = createUserInDB

registerStudentAPI :: User -> IO ()
registerStudentAPI user = do
  createUserInDB user
  userId <- getUserField user "id" :: IO IntWrapper
  insertAllIntoValidationsAppDB [1, extractInt userId]

swapUser :: User -> User -> [User] -> [User]
swapUser _ _ [] = []
swapUser old new (u : userL)
  | old == u = new : swapUser old new userL
  | otherwise = u : swapUser old new userL