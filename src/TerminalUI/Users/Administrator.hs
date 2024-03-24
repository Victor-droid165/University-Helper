module TerminalUI.Users.Administrator
  ( userRegisterUI,
    displayAdministratorOptions,
  )
where

import Lib
  ( displayActionSelectionMessage,
    joinStringArray,
    selectOption,
  )
import Models.User ()
import TerminalUI.Users.User (typeEnrollment, typeUniversity, typeUserEmail, typeUserName, typeUserPassword)
import Util.Constants
  ( administratorOptionsPrompts,
    emailInputPrompts,
    passwordInputPrompt,
    universityInputPrompts,
    userEnrollmentInputPrompts,
    userNameInputPrompts,
  )
import Util.ScreenCleaner (screenCleaner)

userRegisterUI :: IO (String, String, String, String, String, String)
userRegisterUI = do
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

displayAdministratorOptions :: [IO ()] -> IO ()
displayAdministratorOptions actions = do
  displayActionSelectionMessage
  selectOption $ zip administratorOptionsPrompts actions