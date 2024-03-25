{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Models.User
  ( User (..),
    userToString,
    stringToUser,
    writeUserOnFile,
    displayUser,
    setType,
    removeUser,
    showUserAPI,
    showAll,
    getUserValidateList,
    filterByUserEnroll,
  )
where

import Data.Maybe (mapMaybe)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import GHC.Generics (Generic)
import Lib
  ( stringToData,
    writeDataOnFile,
  )
import Util.Database.Functions.UsersDBFunctions (selectAllFromUsersWhereAppDB)

data User = User
  { userType :: String,
    userName :: String,
    userUniversity :: String,
    userEnrollment :: String,
    userEmail :: String,
    userPassword :: String
  }
  deriving (Show, Read, Eq, Generic)

newtype UserEnrollment = UserEnrollment User

instance Eq UserEnrollment where
  (==) :: UserEnrollment -> UserEnrollment -> Bool
  (UserEnrollment user1) == (UserEnrollment user2) = userEnrollment user1 == userEnrollment user2

instance FromRow User

instance ToRow User

userToString :: User -> String
userToString = show

stringToUser :: String -> Maybe User
stringToUser = stringToData

writeUserOnFile :: FilePath -> User -> IO ()
writeUserOnFile = writeDataOnFile

userTypeToString :: User -> String
userTypeToString user
  | userType user == "teacher" = "Professor"
  | userType user == "student" = "Aluno"
  | userType user == "administrator" = "Administrador"
  | otherwise = "NotAValidType"

setType :: String -> User -> User
setType userType' user = user {userType = userType'}

displayUser :: User -> IO ()
displayUser user = do
  putStrLn $ userName user ++ " - " ++ userEnrollment user ++ " (" ++ userTypeToString user ++ ")"
  putStrLn $ userUniversity user
  putStrLn $ userEmail user

showUserAPI :: User -> String
showUserAPI user =
  userName user
    ++ " - "
    ++ userEnrollment user
    ++ " ("
    ++ userTypeToString user
    ++ ")\n"
    ++ userUniversity user
    ++ "\n"
    ++ userEmail user
    ++ "\n"

getUserValidateList :: IO [User]
getUserValidateList = selectAllFromUsersWhereAppDB [("admin_validator_id", "IS", "NULL")]

showAll :: [User] -> String
showAll = concatMap showUserAPI

removeUser :: String -> [User] -> [User]
removeUser _ [] = []
removeUser enroll (u : userList)
  | enroll == userEnrollment u = removeUser enroll userList
  | otherwise = u : removeUser enroll userList

filterByUserEnroll :: String -> [User] -> [User]
filterByUserEnroll _ [] = []
filterByUserEnroll enroll (u : userList)
  | enroll == userEnrollment u = filterByUserEnroll enroll userList
  | otherwise = u : filterByUserEnroll enroll userList
