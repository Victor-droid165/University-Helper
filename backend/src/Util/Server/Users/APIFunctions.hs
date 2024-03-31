module Util.Server.Users.APIFunctions
  ( usersAPIFunctions,
  )
where

import Control.Monad.IO.Class (liftIO)
import Controllers.Users.AdministratorController (getIds, validateUserAPI)
import Controllers.Users.UserController
  ( findUserByEmail,
    getDBUsers,
    getUsers,
    registerStudentAPI,
    registerUserAPI,
    verifyLoginIO,
  )
import Data.Maybe (fromJust)
import Models.AdminValidate (AdminV)
import Models.DB.DBUser (DBUser (..))
import Models.User (User (..), showAll, showUserAPI)
import Servant
import Util.Database.Functions.UsersDBFunctions (selectFromUsersWhereAppDB, updateInUsersWhereAppDB)
import Util.Server.Users.APIDatas
  ( ChangeData (..),
    LogInfo (..),
    RandomData (..),
    RegisterInfo (..),
  )
import Util.Server.Users.APIRoutes (UsersAPI)
import Util.Validate
  ( handleValidationServer,
    userEmailValidation,
    userEnrollmentValidation,
    userNameValidation,
    userPasswordValidation,
    userUniversityValidation,
  )

usersAPIFunctions :: Server Util.Server.Users.APIRoutes.UsersAPI
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

validateLogin :: LogInfo -> Handler Bool
validateLogin logInfo = liftIO $ verifyLoginIO (email logInfo) (password logInfo)

validateUser :: Int -> Handler NoContent
validateUser userId' = liftIO (validateUserAPI userId') >> return NoContent

unvalidateUser :: String -> Handler NoContent
unvalidateUser myData = deleteUser myData >> return NoContent

register :: RegisterInfo -> Handler String
register registerInfo = do
  isUserRegistered <- isRegistered (userEmail (user registerInfo))
  if isUserRegistered
    then return "Failure"
    else do
      liftIO $ registerIn registerInfo
  where
    registerIn registerInfo'
      | u_type registerInfo' == "Professor" = liftIO $ registerUserAPI (user registerInfo') >> return "Success"
      | otherwise = liftIO $ registerStudentAPI (user registerInfo') >> return "Success"

isRegistered :: String -> Handler Bool
isRegistered emailData = do
  allUsers <- liftIO getUsers
  return $ emailData `elem` map userEmail allUsers

showUser :: String -> Handler String
showUser myData = do
  allUsers <- liftIO getUsers
  return $ (showUserAPI . fromJust) (findUserByEmail myData allUsers)

showAllUsers :: Handler String
showAllUsers = do
  allUsers <- liftIO getUsers
  return $ showAll allUsers

deleteUser :: String -> Handler String
deleteUser mydata = do
  if mydata /= "1"
    then do
      liftIO $ updateInUsersWhereAppDB [("is_deleted", "t")] [("id", "=", mydata)]
      -- liftIO $ deleteFromValidationsWhereAppDB [("user_id", "=", stringValue mydata)]
      -- liftIO $ deleteFromUsersWhereAppDB [("id", "=", stringValue mydata)]
      return "Success"
    else
      return "You can't delete yourself, cuz you're the ADMIN"

updateAny :: ChangeData -> Handler String
updateAny changeData = do
  liftIO $ updateInUsersWhereAppDB [(field changeData, newValue changeData)] [(match changeData, "=", matchValue changeData)]
  showUser (matchValue changeData)

getAny :: Maybe String -> Maybe String -> Maybe String -> Handler String
getAny mUniqueKeyName mUniqueKey mAttribute = do
  liftIO $ print mUniqueKeyName
  liftIO $ print mUniqueKey
  liftIO $ print mAttribute
  result <- liftIO (selectFromUsersWhereAppDB [fromJust mAttribute] [(fromJust mUniqueKeyName, "=", fromJust mUniqueKey)] :: IO [RandomData])
  return $ (userType' . head) result

-- LOGIN AND REGISTER

users :: Handler [User]
users = liftIO getUsers

usersDB :: Handler [DBUser]
usersDB = liftIO getDBUsers