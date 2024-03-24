{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE InstanceSigs #-}

module Models.User
  ( User (..),
    userToString,
    stringToUser,
    writeUserOnFile,
    displayUser,
    setType,
    removeUser,
    getUserList,
    verifyLoginIO,
    showUserAPI,
    showAll,
    getUserValidateList,
    filterByUserEnroll,
  )
where

import Lib
  ( stringToData,
    writeDataOnFile,
  )
import GHC.Generics (Generic)

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
      userName user ++ " - " ++ userEnrollment user ++ " (" ++ userTypeToString user ++ ")\n"
      ++ userUniversity user ++ "\n"
      ++ userEmail user ++ "\n"

getUserList :: IO [User]
getUserList = do
    contents <- readFile "data/users.txt"
    return $ mapMaybe stringToUser (lines contents)

getUserValidateList :: IO [User]
getUserValidateList = do
    contents <- readFile "data/toValidate.txt"
    return $ mapMaybe stringToUser (lines contents)

showAll :: [User] -> String
showAll [] = ""
showAll (u:us) = (showUserAPI u) ++ showAll us


getUserByEmail :: String -> [User] -> User
getUserByEmail email [] = User  { userType = "",
    userName = "",
    userUniversity = "",
    userEnrollment = "",
    userEmail = "",
    userPassword = ""
  }
getUserByEmail email (u:userList)    | email == userEmail u = u
                              | otherwise = getUserByEmail email userList

verifyLoginIO :: String -> String -> IO Bool
verifyLoginIO email password = do
  userL <- getUserList
  let user = getUserByEmail email userL
  if userPassword user == password
    then writeUserOnFile "data/session.txt" user >> return True
  else
    return False

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
