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
    showUserAPI,
    showAll,
    fromDBUser,
  )
where

import Data.Aeson.Types (FromJSON, ToJSON)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import GHC.Generics (Generic)
import Lib
  ( stringToData,
    writeDataOnFile,
  )
import Models.DBUser (DBUser (..))

data User = User
  { userName :: String,
    userEmail :: String,
    userPassword :: String,
    userType :: String,
    userEnrollment :: String,
    userUniversity :: String
  }
  deriving (Show, Read, Eq, Generic)

newtype UserEnrollment = UserEnrollment User

instance Eq UserEnrollment where
  (==) :: UserEnrollment -> UserEnrollment -> Bool
  (UserEnrollment user1) == (UserEnrollment user2) = userEnrollment user1 == userEnrollment user2

instance FromRow User

instance ToRow User

instance ToJSON User

instance FromJSON User

userToString :: User -> String
userToString = show

stringToUser :: String -> Maybe User
stringToUser = stringToData

writeUserOnFile :: FilePath -> User -> IO ()
writeUserOnFile = writeDataOnFile

userTypeToString :: User -> String
userTypeToString user
  | userType user == "Professor" = "Professor"
  | userType user == "Student" = "Aluno"
  | userType user == "Admin" = "Administrador"
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

fromDBUser :: DBUser -> User
fromDBUser dbUser =
  User
    { userName = dbUserName dbUser,
      userEmail = dbUserEmail dbUser,
      userPassword = dbUserPassword dbUser,
      userType = dbUserType dbUser,
      userEnrollment = dbUserEnrollment dbUser,
      userUniversity = dbUserUniversity dbUser
    }


showAll :: [User] -> String
showAll = concatMap showUserAPI