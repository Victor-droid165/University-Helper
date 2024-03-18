module Controllers.Users.UserController
  ( userRegister,
    userLogin,
  )
where

import Controllers.Users.AdministratorController (administratorOptions)
import Controllers.Users.StudentController (studentOptions)
import Controllers.Users.TeacherController (teacherOptions)
import Data.Maybe (mapMaybe)
import Lib (joinStringArray)
import Models.User
import System.Directory (removeFile)
import TerminalUI.Users.User (loginUI, registerUI, typeUserEmail, typeUserPassword)
import Util.ScreenCleaner (screenCleaner)

userRegister :: IO ()
userRegister = do
  (userType', userName', userUniversity', userEnrollment', userEmail', userPassword') <- registerUI
  let newUser =
        User
          { userType = userType',
            userName = userName',
            userUniversity = userUniversity',
            userEnrollment = userEnrollment',
            userEmail = userEmail',
            userPassword = userPassword'
          }

  toValidate newUser
  displayUser newUser

toValidate :: User -> IO ()
toValidate user
  | userType user == "teacher" = do
      writeUserOnFile "data/toValidate.txt" user
      putStrLn "Usuário aguardando validação!"
  | otherwise = do
      writeUserOnFile "data/users.txt" user
      putStrLn "Usuário registrado com sucesso!"

updateUser :: User -> IO ()
updateUser user = do
  content <- readFile "data/users.txt"
  let userList = mapMaybe stringToUser (lines content)
  let oldUser = getUser (userEnrollment user) userList

  let newUserL = swapUser oldUser user userList
  removeFile "data/users.txt"
  mapM_ (writeUserOnFile "data/users.txt") newUserL

  screenCleaner
  putStrLn "O seguinte usuário foi atualizado com sucesso: "
  displayUser user
  administratorOptions

swapUser :: User -> User -> [User] -> [User]
swapUser _ _ [] = []
swapUser old new (u : userL)
  | old == u = new : swapUser old new userL
  | otherwise = u : swapUser old new userL

getUser :: String -> [User] -> User
getUser _ [] = User {}
getUser enroll (u : userList)
  | enroll == userEnrollment u = u
  | otherwise = getUser enroll userList

updateEmailPassword :: User -> IO ()
updateEmailPassword user = do
  screenCleaner
  newEmail <-
    typeUserEmail
      ( joinStringArray
          [ "Como esse é um email padrão, será necessário atualiza-lo",
            "Digite um NOVO E-MAIL que constará no sistema:"
          ]
          "\n"
      )
      "register"
  newPassword <-
    typeUserPassword $
      joinStringArray
        [ "Como essa é uma senha padrão, será necessário atualizá-la",
          "Digite uma NOVA SENHA que constará no sistema:"
        ]
        "\n"

  let updatedUser = user {userEmail = newEmail, userPassword = newPassword}
  updateUser updatedUser
  putStr "Login e senha atualizados com sucesso!"

userLogin :: IO ()
userLogin = do
  (userEmail', userPassword') <- loginUI
  maybeUser <- authenticateUser userEmail' userPassword'
  handleUser maybeUser userEmail' userPassword'
  where
    handleUser :: Maybe User -> String -> String -> IO ()
    handleUser (Just user) userEmail' userPassword'
      | userEmail' == "everton@admin.ufcg.edu.br" && userPassword' == "senhasegura" =
          updateEmailPassword user
      | otherwise = do
          writeUserOnFile "data/session.txt" user
          handleRegularUser user
    handleUser Nothing _ _ = putStrLn "Invalid email or password."

    handleRegularUser :: User -> IO ()
    handleRegularUser user
      | userType user == "administrator" = administratorOptions
      | userType user == "student" = studentOptions
      | userType user == "teacher" = teacherOptions
      | otherwise = putStrLn "Invalid email or password."
