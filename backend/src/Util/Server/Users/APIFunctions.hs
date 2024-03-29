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
import Models.DBUser (DBUser (..))
import Models.User (User (..), showAll, showUserAPI)
import Servant
import Util.Database.Functions.UsersDBFunctions (selectFromUsersWhereAppDB, updateInUsersWhereAppDB)
import Util.Server.Users.APIDatas
  ( ChangeData (..),
    IntegerData (..),
    LogInfo (..),
    MyData (..),
    RandomData (..),
    RegisterInfo (..),
  )
import Util.Server.Users.APIRoutes (UsersAPI (..))
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
  usersG
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

validateName :: MyData -> Handler String
validateName myData = return $ handleValidationServer (userNameValidation (value myData))

validateUniversity :: MyData -> Handler String
validateUniversity myData = return $ handleValidationServer (userUniversityValidation (value myData))

validateEmail :: MyData -> Handler String
validateEmail myData = return $ handleValidationServer (userEmailValidation (value myData))

validateEnrollment :: MyData -> Handler String
validateEnrollment myData = return $ handleValidationServer (userEnrollmentValidation (value myData))

validatePassword :: MyData -> Handler String
validatePassword myData = return $ handleValidationServer (userPasswordValidation (value myData))

validateLogin :: LogInfo -> Handler Bool
validateLogin logInfo = liftIO $ verifyLoginIO (email logInfo) (password logInfo)

validateUser :: IntegerData -> Handler NoContent
validateUser myData = liftIO (validateUserAPI (integerValue myData)) >> return NoContent

unvalidateUser :: MyData -> Handler NoContent
unvalidateUser myData = deleteUser myData >> return NoContent

register :: RegisterInfo -> Handler String
register registerInfo = do
  isUserRegistered <- isRegistered (MyData {value = userEmail (user registerInfo)})
  if isUserRegistered
    then return "Failure"
    else do
      liftIO $ registerIn registerInfo
  where
    registerIn registerInfo'
      | u_type registerInfo' == "Professor" = liftIO $ registerUserAPI (user registerInfo') >> return "Success"
      | otherwise = liftIO $ registerStudentAPI (user registerInfo') >> return "Success"

isRegistered :: MyData -> Handler Bool
isRegistered emailData = do
  let email' = value emailData
  allUsers <- liftIO getUsers
  return $ email' `elem` map userEmail allUsers

showUser :: MyData -> Handler String
showUser myData = do
  users <- liftIO getUsers
  return $ (showUserAPI . fromJust) (findUserByEmail (value myData) users)

showAllUsers :: Handler String
showAllUsers = do
  users <- liftIO getUsers
  return $ showAll users

deleteUser :: MyData -> Handler String
deleteUser mydata = do
  if value mydata /= "1"
    then do
      liftIO $ updateInUsersWhereAppDB [("is_deleted", "t")] [("id", "=", value mydata)]
      -- liftIO $ deleteFromValidationsWhereAppDB [("user_id", "=", value mydata)]
      -- liftIO $ deleteFromUsersWhereAppDB [("id", "=", value mydata)]
      return "Success"
    else
      return "You can't delete yourself, cuz you're the ADMIN"

updateAny :: ChangeData -> Handler String
updateAny mydata = do
  liftIO $ updateInUsersWhereAppDB [(field mydata, newValue mydata)] [(match mydata, "=", matchValue mydata)]
  showUser (MyData {value = matchValue mydata})

getAny :: Maybe String -> Maybe String -> Maybe String -> Handler String
getAny mUniqueKeyName mUniqueKey mAttribute = do
  liftIO $ print mUniqueKeyName
  liftIO $ print mUniqueKey
  liftIO $ print mAttribute
  result <- liftIO (selectFromUsersWhereAppDB [fromJust mAttribute] [(fromJust mUniqueKeyName, "=", fromJust mUniqueKey)] :: IO [RandomData])
  return $ (userType' . head) result

-- LOGIN AND REGISTER

usersG :: Handler [User]
usersG = liftIO getUsers

usersDB :: Handler [DBUser]
usersDB = liftIO getDBUsers