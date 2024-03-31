{-# LANGUAGE BinaryLiterals #-}

module Controllers.Users.UserController
  ( userRegister,
    userLogin,
    getUsers,
    autoRemove,
    swapUser,
    removeUser,
    verifyLoginIO,
    getLoggedUser,
    findUserByEmail,
    findUserByEnroll,
    registerUserAPI,
    getDBUsers,
    registerStudentAPI,
    getUserById,
  )
where

import Data.Foldable (find)
import Data.Maybe (fromJust, mapMaybe)
import Database.PostgreSQL.Simple.Types (Only (..))
import Lib (handleMaybe, joinStringArray, selectOption)
import Models.DB.DBUser (DBUser)
import Models.User
  ( User (..),
    displayUser,
    setType,
    stringToUser,
    writeUserOnFile,
  )
import Repositories.UserRepository (createUserInDB, getDBusersFromDB, getUserFromDBWhere, getUsersFromDB, removeUserFromDBByEnroll, updateUserInDB)
import System.Directory (removeFile)
import TerminalUI.Users.Administrator (displayAdministratorOptions, userRegisterUI)
import TerminalUI.Users.Student (displayStudentOptions)
import TerminalUI.Users.Teacher (displayTeacherOptions)
import TerminalUI.Users.User (loginUI, registerUI, typeEnrollment, typeUserEmail, typeUserPassword)
import Util.Database.Functions.UsersDBFunctions (selectFromUsersWhereAppDB)
import Util.Database.Functions.ValidationDBFunctions (insertAllIntoValidationsAppDB)
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
updateUser = updateUserInDB

swapUser :: User -> User -> [User] -> [User]
swapUser _ _ [] = []
swapUser old new (u : userL)
  | old == u = new : swapUser old new userL
  | otherwise = u : swapUser old new userL

getUserById :: Int -> IO User
getUserById userId = getUserFromDBWhere [("id", "=", userId)]

getUsers :: IO [User]
getUsers = getUsersFromDB

getDBUsers :: IO [DBUser]
getDBUsers = getDBusersFromDB

getLoggedUser :: IO (Maybe User)
getLoggedUser = do
  loggedUsers <- readFile "backend/data/session.txt"
  return $ stringToUser (head (lines loggedUsers))

removeUser :: String -> IO ()
removeUser = removeUserFromDBByEnroll

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
      let updatedUser = findUserByEmail (userEnrollment user) userList
      writeUserOnFile "backend/data/session.txt" (fromJust updatedUser)
      return updatedUser
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
  let user = findUserByEnroll enroll userList

  screenCleaner
  newType <- selectOption $ zip ["ADMINISTRADOR", "PROFESSOR", "ALUNO"] [return "administrator", return "teacher", return "student"]
  let newUser = setType newType (fromJust user)
  let newUserL = swapUser (fromJust user) newUser userList
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
  let placeHolderUser = findUserByEmail enroll userList
  let newValidateList = filterByUserEnroll enroll userList
  removeFile "backend/data/toValidate.txt"
  mapM_ (writeUserOnFile "backend/data/toValidate.txt") newValidateList
  writeUserOnFile "backend/data/users.txt" (fromJust placeHolderUser)

  --
  screenCleaner
  putStrLn "O seguinte usuário foi validado com sucesso: "
  (displayUser . fromJust) placeHolderUser
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
  allUsers <- getUsers
  let user = findUserByEmail email allUsers
      userPassword' = handleMaybe user "" userPassword
  tryToPerformLogin userPassword'
  where
    tryToPerformLogin :: String -> IO Bool
    tryToPerformLogin userPassword'
      | userPassword' == password = return True
      | otherwise = return False

findUserByEmail :: String -> [User] -> Maybe User
findUserByEmail _ [] = Nothing
findUserByEmail email (user : users)
  | email == userEmail user = Just user
  | otherwise = findUserByEmail email users

findUserByEnroll :: String -> [User] -> Maybe User
findUserByEnroll _ [] = Nothing
findUserByEnroll enroll (user : users)
  | enroll == userEnrollment user = Just user
  | otherwise = findUserByEnroll enroll users

filterByUserEnroll :: String -> [User] -> [User]
filterByUserEnroll _ [] = []
filterByUserEnroll enroll (u : userList)
  | enroll == userEnrollment u = filterByUserEnroll enroll userList
  | otherwise = u : filterByUserEnroll enroll userList

registerUserAPI :: User -> IO ()
registerUserAPI = createUserInDB

registerStudentAPI :: User -> IO ()
registerStudentAPI user = do
  createUserInDB user
  [userId] <- selectFromUsersWhereAppDB ["id"] [("email", "=", userEmail user)]
  let (Only userIdValue) = userId
  insertAllIntoValidationsAppDB [1 :: Integer, userIdValue]
