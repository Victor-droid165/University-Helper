module Controllers.Users.UserController
  ( userRegister,
    userLogin,
    getUsers,
    getUser,
    autoRemove,
    swapUser,
    removeUser,
    verifyLoginIO,
    getLoggedUser,
  )
where

import DBLib (AnyField (..))
import Data.Foldable (find)
import Data.Maybe (mapMaybe)
import Lib (handleMaybe, joinStringArray, selectOption)
import Models.User
  ( User (..),
    displayUser,
    filterByUserEnroll,
    fromDBUser,
    setType,
    stringToUser,
    writeUserOnFile,
  )
import System.Directory (removeFile)
import TerminalUI.Users.Administrator (displayAdministratorOptions, userRegisterUI)
import TerminalUI.Users.Student (displayStudentOptions)
import TerminalUI.Users.Teacher (displayTeacherOptions)
import TerminalUI.Users.User (loginUI, registerUI, typeEnrollment, typeUserEmail, typeUserPassword)
import Util.Database.Functions.UsersDBFunctions (deleteFromUsersWhereAppDB, selectAllFromUsersAppDB, updateAllInUsersWhereAppDB)
import Util.ScreenCleaner (forceQuit, quitIO, screenCleaner)

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
  processUserOptions newUser

toValidate :: User -> IO ()
toValidate user
  | userType user == "teacher" = do
      writeUserOnFile "backend/data/toValidate.txt" user
      putStrLn "Usuário aguardando validação!"
  | otherwise = do
      writeUserOnFile "backend/data/users.txt" user
      putStrLn "Usuário registrado com sucesso!"

updateUser :: User -> IO ()
updateUser user = do
  let newValues = [userName user, userEmail user, userPassword user, userEnrollment user, userType user, userUniversity user]
  updateAllInUsersWhereAppDB newValues [("enrollment_number", "=", userEnrollment user)]

swapUser :: User -> User -> [User] -> [User]
swapUser _ _ [] = []
swapUser old new (u : userL)
  | old == u = new : swapUser old new userL
  | otherwise = u : swapUser old new userL

getUsers :: IO [User]
getUsers = map fromDBUser <$> selectAllFromUsersAppDB

getUser :: String -> [User] -> User
getUser _ [] = User {}
getUser enroll (u : userList)
  | enroll == userEnrollment u = u
  | otherwise = getUser enroll userList

getLoggedUser :: IO (Maybe User)
getLoggedUser = do
  loggedUsers <- readFile "backend/data/session.txt"
  return $ stringToUser (head (lines loggedUsers))

removeUser :: String -> IO ()
removeUser enroll = deleteFromUsersWhereAppDB [("enrollment_number", "=", enroll)]

autoRemove :: IO ()
autoRemove = do
  userToRemove <- getLoggedUser
  handleMaybe userToRemove (return ()) (removeUser . userEnrollment)

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
  maybeLoggedUser <- handleUserLogin maybeUser userEmail' userPassword'
  handleMaybe maybeLoggedUser (return ()) processUserOptions

processUserOptions :: User -> IO ()
processUserOptions user
  | userType user == "teacher" = displayTeacherOptions [autoRemove >> forceQuit (processUserOptions user), quitIO (processUserOptions user)]
  | userType user == "student" = displayStudentOptions [autoRemove >> forceQuit (processUserOptions user), quitIO (processUserOptions user)]
  | userType user == "administrator" = administratorOptions
  | otherwise = return ()

handleUserLogin :: Maybe User -> String -> String -> IO (Maybe User)
handleUserLogin (Just user) userEmail' userPassword'
  | userEmail' == "everton@admin.ufcg.edu.br" && userPassword' == "senhasegura" = do
      updateEmailPassword user
      userList <- getUsers
      let updatedUser = getUser (userEnrollment user) userList
      writeUserOnFile "backend/data/session.txt" updatedUser
      return (Just updatedUser)
  | otherwise = do
      writeUserOnFile "backend/data/session.txt" user
      return (Just user)
handleUserLogin Nothing _ _ = return Nothing

authenticateUser :: String -> String -> IO (Maybe User)
authenticateUser email password =
  find (\user -> userEmail user == email && userPassword user == password) <$> getUsers

userRegisterADMIN :: IO ()
userRegisterADMIN = do
  (adminType, name, university, enrollment, email, password) <- userRegisterUI
  let newUser =
        User
          { userType = adminType,
            userName = name,
            userUniversity = university,
            userEnrollment = enrollment,
            userEmail = email,
            userPassword = password
          }
  writeUserOnFile "backend/data/users.txt" newUser
  putStrLn "Usuário registrado com sucesso!"
  administratorOptions

updateUserAdmin :: IO ()
updateUserAdmin = do
  screenCleaner
  putStrLn "Digite a matrícula do Usuário que pretende alterar o tipo: "
  enroll <- getLine
  content <- readFile "backend/data/users.txt"
  let userList = mapMaybe stringToUser (lines content)
  let user = getUser enroll userList

  screenCleaner
  newType <- selectOption $ zip ["ADMINISTRADOR", "PROFESSOR", "ALUNO"] [return "administrator", return "teacher", return "student"]
  let newUser = setType newType user
  let newUserL = swapUser user newUser userList
  removeFile "backend/data/users.txt"
  mapM_ (writeUserOnFile "backend/data/users.txt") newUserL

  screenCleaner
  putStrLn "O seguinte usuário foi atualizado com sucesso: "
  displayUser newUser
  administratorOptions

validateUser :: IO ()
validateUser = do
  screenCleaner
  content <- readFile "backend/data/toValidate.txt"
  let userList = mapMaybe stringToUser (lines content)
  mapM_ displayUser userList
  putStrLn "Digite a matrícula do usuário que deseja validar: "
  enroll <- getLine
  --
  let placeHolderUser = getUser enroll userList
  let newValidateList = filterByUserEnroll enroll userList
  removeFile "backend/data/toValidate.txt"
  mapM_ (writeUserOnFile "backend/data/toValidate.txt") newValidateList
  writeUserOnFile "backend/data/users.txt" placeHolderUser

  --
  screenCleaner
  putStrLn "O seguinte usuário foi validado com sucesso: "
  displayUser placeHolderUser
  administratorOptions

userRemove :: IO ()
userRemove = do
  enroll <-
    typeEnrollment $
      joinStringArray
        [ "Agora precisamos saber qual a matrícula do usuário",
          "Digite o numero de MATRÍCULA da pessoa que você removerá do sistema(isso pode incluir você mesmo):"
        ]
        "\n"

  currentUser <- getLoggedUser
  let currentUserEnrollment = maybe "" userEnrollment currentUser

  if enroll == currentUserEnrollment
    then do
      autoRemove
      forceQuit administratorOptions
    else do
      removeUser enroll
      administratorOptions

administratorOptions :: IO ()
administratorOptions = displayAdministratorOptions [userRegisterADMIN, userRemove, updateUserAdmin, validateUser, quitIO administratorOptions]

verifyLoginIO :: String -> String -> IO Bool
verifyLoginIO email password = do
  userL <- getUsers
  let user = getUserByEmail email userL
  print user
  if userPassword user == password
    then do
      writeUserOnFile "backend/data/session.txt" user
      return True
    else
      return False

getUserByEmail :: String -> [User] -> User
getUserByEmail _ [] =
  User
    { userType = "",
      userName = "",
      userUniversity = "",
      userEnrollment = "",
      userEmail = "",
      userPassword = ""
    }
getUserByEmail email (u : userList)
  | email == userEmail u = u
  | otherwise = getUserByEmail email userList