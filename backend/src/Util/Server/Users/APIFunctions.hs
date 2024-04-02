module Util.Server.Users.APIFunctions
  ( usersAPIFunctions,
  )
where

import Control.Monad.IO.Class (liftIO)
import Controllers.Users.AdministratorController (getIds, validateUserAPI)
import Controllers.Users.UserController
  ( getDBUsers,
    getUserByEmail,
    getUsers,
    registerStudentAPI,
    registerUserAPI,
    verifyLoginIO,
  )
import Data.Maybe (fromJust)
import Models.AdminValidate (AdminV)
import Models.DB.DBUpdateValue (DBUpdateValue (..))
import Models.DB.DBUser (DBUser (..), UserLogInfo (..))
import Models.User (User (..), fromDBUser, showAll, showUserAPI)
import Models.WrapperTypes.StringWrapper (StringWrapper (..), extractString)
import Servant
import Util.Database.Functions.UsersDBFunctions (deleteFromUsersWhereAppDB, selectAllFromUsersWhereAppDB, selectFromUsersWhereAppDB, updateInUsersWhereAppDB)
import Util.Server.Users.APIRoutes (UsersAPI)
import Util.Validate
  ( handleValidationServer,
    userEmailValidation,
    userEnrollmentValidation,
    userNameValidation,
    userPasswordValidation,
    userUniversityValidation,
  )

usersAPIFunctions :: Server UsersAPI
usersAPIFunctions =
  users
    :<|> usersDB
    :<|> getIdsValidated
    :<|> validateName
    :<|> validateUniversity
    :<|> validateEmail
    :<|> validateEnrollment
    :<|> validatePassword
    :<|> validateLogin
    :<|> validateUser
    :<|> unvalidateUser
    :<|> register
    :<|> isRegistered
    :<|> showUser
    :<|> showAllUsers
    :<|> deleteUser
    :<|> updateAny
    :<|> getAny
    :<|> getUser

getIdsValidated :: Handler [AdminV]
getIdsValidated = liftIO getIds

validateName :: String -> Handler String
validateName userName' = return $ handleValidationServer (userNameValidation userName')

validateUniversity :: String -> Handler String
validateUniversity universityName = return $ handleValidationServer (userUniversityValidation universityName)

validateEmail :: String -> Handler String
validateEmail email' = return $ handleValidationServer (userEmailValidation email')

validateEnrollment :: String -> Handler String
validateEnrollment enrollment = return $ handleValidationServer (userEnrollmentValidation enrollment)

validatePassword :: String -> Handler String
validatePassword password' = return $ handleValidationServer (userPasswordValidation password')

validateLogin :: UserLogInfo -> Handler Bool
validateLogin logInfo = liftIO $ verifyLoginIO (email logInfo) (password logInfo)

validateUser :: Int -> Handler NoContent
validateUser userId' = do
  liftIO (validateUserAPI userId')
  return NoContent

unvalidateUser :: String -> Handler NoContent
unvalidateUser myData = do
  deleteUser myData
  return NoContent

register :: User -> Handler String
register user = do
  isUserRegistered <- isRegistered (userEmail user)
  if isUserRegistered
    then return "Failure"
    else
      liftIO $ registerIn user
  where
    registerIn user'
      | userType user' == "Professor" = do
          liftIO $ registerUserAPI user'
          return "Success"
      | otherwise = do
          liftIO $ registerStudentAPI user'
          return "Success"

isRegistered :: String -> Handler Bool
isRegistered emailData = do
  allUsers <- liftIO getUsers
  return $ emailData `elem` map userEmail allUsers

showUser :: String -> Handler String
showUser email' = do
  user <- liftIO $ getUserByEmail email'
  return $ showUserAPI user

showAllUsers :: Handler String
showAllUsers = liftIO $ showAll <$> getUsers

deleteUser :: String -> Handler String
deleteUser userId = do
  if userId /= "1"
    then do
      -- For soft deletions
      -- liftIO $ updateInUsersWhereAppDB [("is_deleted", "t")] [("id", "=", mydata)]
      liftIO $ deleteFromUsersWhereAppDB [("id", "=", userId)]
      return "Success"
    else
      return "You can't delete yourself, cuz you're the ADMIN"

updateAny :: DBUpdateValue -> Handler String
updateAny updateData = do
  liftIO $ updateInUsersWhereAppDB [(fieldToUpdate updateData, newValue updateData)] [(whereField updateData, "=", whereValue updateData)]
  showUser (whereValue updateData)

getAny :: Maybe String -> Maybe String -> Maybe String -> Handler String
getAny mUniqueKeyName mUniqueKey mAttribute = do
  result <- liftIO (selectFromUsersWhereAppDB [fromJust mAttribute] [(fromJust mUniqueKeyName, "=", fromJust mUniqueKey)] :: IO [StringWrapper])
  return $ (extractString . head) result

getUser :: Maybe String -> Maybe String -> Handler User
getUser mUniqueKeyName mUniqueKey = do
  [user] <- liftIO (selectAllFromUsersWhereAppDB [(fromJust mUniqueKeyName, "=", fromJust mUniqueKey)])
  return $ fromDBUser (user :: DBUser)

-- LOGIN AND REGISTER

users :: Handler [User]
users = liftIO getUsers

usersDB :: Handler [DBUser]
usersDB = liftIO getDBUsers