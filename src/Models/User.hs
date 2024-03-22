{-# LANGUAGE DeriveGeneric #-}
module Models.User
  ( User (..),
    authenticateUser,
    userToString,
    stringToUser,
    writeUserOnFile,
    displayUser,
    setType,
    removeUser,
    
  )
where

import Data.Foldable (find)
import Data.Maybe (mapMaybe)
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
  (UserEnrollment user1) == (UserEnrollment user2) = userEnrollment user1 == userEnrollment user2

authenticateUser :: String -> String -> IO (Maybe User)
authenticateUser email password = do
  content <- readFile "data/users.txt"
  let users = mapMaybe stringToUser (lines content)
  return $ find (\user -> userEmail user == email && userPassword user == password) users

userToString :: User -> String
userToString = show

stringToUser :: String -> Maybe User
stringToUser line = stringToData line

writeUserOnFile :: FilePath -> User -> IO ()
writeUserOnFile filePath user = writeDataOnFile filePath user

userTypeToString :: User -> String
userTypeToString user
  | userType user == "teacher" = "Professor"
  | userType user == "student" = "Aluno"
  | userType user == "administrator" = "Administrador"
  | otherwise = "NotAValidType"

setType :: String -> User -> User
setType userType user = user {userType = userType}

displayUser :: User -> IO ()
displayUser user = do
  putStrLn $ userName user ++ " - " ++ userEnrollment user ++ " (" ++ userTypeToString user ++ ")"
  putStrLn $ userUniversity user
  putStrLn $ userEmail user

removeUser :: String -> [User] -> [User]
removeUser _ [] = []
removeUser enroll (u : userList)
  | enroll == userEnrollment u = removeUser enroll userList
  | otherwise = u : removeUser enroll userList