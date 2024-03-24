module Controllers.Users.AdministratorController
  ( validateUserAPI,
    unvalidateUserAPI,
  )
where

import Controllers.Users.UserController (getUser, removeUser)
import Data.Maybe (mapMaybe)
import Models.User (filterByUserEnroll, stringToUser, writeUserOnFile)
import System.Directory (removeFile)

validateUserAPI :: String -> IO ()
validateUserAPI enrollment = do
  contents <- readFile "backend/data/toValidate.txt"
  let validateList = mapMaybe stringToUser (lines contents)

  let placeHolderUser = getUser enrollment validateList
  let newValidateList = filterByUserEnroll enrollment validateList

  removeFile "backend/data/toValidate.txt"
  mapM_ (writeUserOnFile "backend/data/toValidate.txt") newValidateList
  writeUserOnFile "backend/data/users.txt" placeHolderUser

unvalidateUserAPI :: String -> IO ()
unvalidateUserAPI enrollment = do
  contents <- readFile "backend/data/toValidate.txt"
  let validateList = mapMaybe stringToUser (lines contents)
  let newValidateList = filterByUserEnroll enrollment validateList

  removeFile "backend/data/toValidate.txt"
  mapM_ (writeUserOnFile "backend/data/toValidate.txt") newValidateList