module Controllers.Users.AdministratorController
  ( administratorOptions,
  )
where

import Data.Foldable (find)
import Data.Maybe (mapMaybe)
import Lib (joinStringArray, selectOption)
import Models.User
import System.Directory
import TerminalUI.Users.Administrator (displayActionSelection, userRegister)
import TerminalUI.Users.User (registerUI, typeEnrollment)
import Util.ScreenCleaner (forceQuit, quitIO, screenCleaner)

userRegisterADMIN :: IO ()
userRegisterADMIN = do
  (adminType, name, university, enrollment, email, password) <- userRegister
  let newUser =
        User
          { userType = adminType,
            userName = name,
            userUniversity = university,
            userEnrollment = enrollment,
            userEmail = email,
            userPassword = password
          }
  writeUserOnFile "data/users.txt" newUser
  putStrLn "Usuário registrado com sucesso!"
  administratorOptions

updateUser :: IO ()
updateUser = do
  screenCleaner
  putStrLn "Digite a matrícula do Usuário que pretende alterar o tipo: "
  enroll <- getLine
  content <- readFile "data/users.txt"
  let userList = mapMaybe stringToUser (lines content)
  let user = getUser enroll userList

  screenCleaner
  newType <- selectOption $ zip ["ADMINISTRADOR", "PROFESSOR", "ALUNO"] [return "administrator", return "teacher", return "student"]
  let newUser = setType newType user
  let newUserL = swapUser user newUser userList
  removeFile "data/users.txt"
  mapM_ (writeUserOnFile "data/users.txt") newUserL

  screenCleaner
  putStrLn "O seguinte usuário foi atualizado com sucesso: "
  displayUser newUser
  administratorOptions

swapUser :: User -> User -> [User] -> [User]
swapUser _ _ [] = []
swapUser old new (u : userL)
  | old == u = new : swapUser old new userL
  | otherwise = u : swapUser old new userL

validateUser :: IO ()
validateUser = do
  screenCleaner
  content <- readFile "data/toValidate.txt"
  let userList = mapMaybe stringToUser (lines content)
  mapM_ displayUser userList
  putStrLn "Digite a matrícula do usuário que deseja validar: "
  enroll <- getLine
  --
  let placeHolderUser = getUser enroll userList
  let newValidateList = removeUser enroll userList
  removeFile "data/toValidate.txt"
  mapM_ (writeUserOnFile "data/toValidate.txt") newValidateList
  writeUserOnFile "data/users.txt" placeHolderUser

  --
  screenCleaner
  putStrLn "O seguinte usuário foi validado com sucesso: "
  displayUser placeHolderUser
  administratorOptions

getUser :: String -> [User] -> User
getUser _ [] = User {}
getUser enroll (u : userList)
  | enroll == userEnrollment u = u
  | otherwise = getUser enroll userList

userRemove :: IO ()
userRemove = do
  enroll <-
    typeEnrollment $
      joinStringArray
        [ "Agora precisamos saber qual a matrícula do usuário",
          "Digite o numero de MATRÍCULA da pessoa que você removerá do sistema(isso pode incluir você mesmo):"
        ]
        "\n"
  content <- readFile "data/users.txt"

  loggedUser <- readFile "data/session.txt"
  let currentUser = stringToUser (head (lines loggedUser))
  let userLoggedEnrollment = maybe "" userEnrollment currentUser

  let userList = mapMaybe stringToUser (lines content)

  if enroll == userLoggedEnrollment
    then autoRemove
    else do
      let newUserList = removeUser enroll userList
      removeFile "data/users.txt"
      mapM_ (writeUserOnFile "data/users.txt") newUserList
      administratorOptions

autoRemove :: IO ()
autoRemove = do
  loggedUser <- readFile "data/session.txt"
  let userToRemove = stringToUser (head (lines loggedUser))
  let enrollment = maybe "" userEnrollment userToRemove

  content <- readFile "data/users.txt"
  let userList = mapMaybe stringToUser (lines content)
  let newUserList = removeUser enrollment userList

  removeFile "data/users.txt"
  mapM_ (writeUserOnFile "data/users.txt") newUserList
  forceQuit administratorOptions

administratorOptions :: IO ()
administratorOptions = do
  actionPrompts <- displayActionSelection
  selectOption $ zip actionPrompts [userRegisterADMIN, userRemove, updateUser, validateUser, quitIO administratorOptions]