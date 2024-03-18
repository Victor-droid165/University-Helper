module TerminalUI.Users.Administrator
  ( displayActionSelection,
    userRegister,
  )
where

import Lib (emailInputPrompts, joinStringArray, passwordInputPrompt, selectOption, universityInputPrompts, userEnrollmentInputPrompts, userNameInputPrompts)
import Models.User
import TerminalUI.Users.User (typeEnrollment, typeUniversity, typeUserEmail, typeUserName, typeUserPassword)
import Util.ScreenCleaner (screenCleaner)

userRegister :: IO (String, String, String, String, String, String)
userRegister = do
  userType <- selectOption $ zip ["ADMINISTRADOR", "PROFESSOR", "ALUNO"] [return "administrator", return "teacher", return "student"]
  screenCleaner

  userName <- typeUserName $ joinStringArray userNameInputPrompts "\n"
  screenCleaner

  userUniversity <- typeUniversity $ joinStringArray universityInputPrompts "\n"
  screenCleaner

  userEnrollment <- typeEnrollment $ joinStringArray userEnrollmentInputPrompts "\n"
  screenCleaner

  userEmail <- typeUserEmail (joinStringArray emailInputPrompts "\n") "register"

  userPassword <- typeUserPassword passwordInputPrompt
  screenCleaner

  return (userType, userName, userUniversity, userEnrollment, userEmail, userPassword)

actions :: [String]
actions =
  [ "CADASTRAR USUARIO",
    "REMOVER USUARIO DO SISTEMA",
    "ATUALIZAR INFORMACOES DE UM USUARIO",
    "VALIDAR CADASTROS",
    "SAIR"
  ]

displayActionSelection :: IO [String]
displayActionSelection = do
  screenCleaner
  putStrLn "Qual tipo de operacao voce gostaria de realizar no momento?"
  return actions
