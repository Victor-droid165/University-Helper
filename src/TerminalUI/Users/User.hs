{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TerminalUI.Users.User
  ( loginUI,
    registerUI,
    typeUserName,
    typeUniversity,
    typeEnrollment,
    typeUserEmail,
    typeUserPassword,
  )
where

import Data.Maybe (catMaybes)
import Lib
  ( getInput,
    getValidInput,
    handleValidation,
    joinStringArray,
    selectOption,
  )
import Models.User (User (..), stringToUser)
import Util.Constants
  ( emailInputPrompts,
    passwordInputPrompt,
    universityInputPrompts,
    userEnrollmentInputPrompts,
    userNameInputPrompts,
  )
import Util.ScreenCleaner (screenCleaner)
import Util.Validate (belongsToList, userEnrollmentValidation, userLoginEmailValidation, userNameValidation, userPasswordValidation, userRegisterEmailValidation, userUniversityValidation)

typeUserName :: String -> IO String
typeUserName textToShow = getValidInput (Just textToShow) userNameValidation

typeUniversity :: String -> IO String
typeUniversity textToShow = getValidInput (Just textToShow) userUniversityValidation

typeEnrollment :: String -> IO String
typeEnrollment textToShow = getValidInput (Just textToShow) userEnrollmentValidation

typeUserEmail :: String -> String -> IO String
typeUserEmail textToShow "login" = getValidInput (Just textToShow) userLoginEmailValidation
typeUserEmail textToShow "register" = do
  email <- getInput (Just textToShow)
  handleValidation (userRegisterEmailValidation email) (continue email) (typeUserEmail textToShow "register")
  where
    continue email = do
      content1 <- readFile "data/users.txt"
      content2 <- readFile "data/toValidate.txt"
      let usersList = [stringToUser line | line <- lines (content1 ++ content2)]
          validEmails = [userEmail user | user <- catMaybes usersList]
      handleValidation (belongsToList validEmails email) (return email) (typeUserEmail textToShow "register")

typeUserPassword :: String -> IO String
typeUserPassword textToShow = getValidInput (Just textToShow) userPasswordValidation

registerUI :: IO (String, String, String, String, String, String)
registerUI = do
  userType' <- selectOption $ zip ["PROFESSOR", "ALUNO"] [return "teacher", return "student"]
  screenCleaner

  userName' <- typeUserName $ joinStringArray userNameInputPrompts "\n"
  screenCleaner

  userUniversity' <- typeUniversity $ joinStringArray universityInputPrompts "\n"
  screenCleaner

  userEnrollment' <- typeEnrollment $ joinStringArray userEnrollmentInputPrompts "\n"
  screenCleaner

  userEmail' <- typeUserEmail (joinStringArray emailInputPrompts "\n") "register"

  userPassword' <- typeUserPassword passwordInputPrompt
  screenCleaner

  return (userType', userName', userUniversity', userEnrollment', userEmail', userPassword')

loginUI :: IO (String, String)
loginUI = do
  screenCleaner
  putStrLn "Bem Vindo ao Login !"
  userEmail' <- typeUserEmail "E-mail: " "login"
  userPassword' <- typeUserPassword "Senha: "
  return (userEmail', userPassword')