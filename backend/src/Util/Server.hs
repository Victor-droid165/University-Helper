{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Util.Server
  ( serveOn,
  )
where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Controllers.Users.AdministratorController (getIds, unvalidateUserAPI, validateUserAPI)
import Controllers.Users.UserController
  ( findUserByEmail,
    getDBUsers,
    getUsers,
    registerStudentAPI,
    registerUserAPI,
    verifyLoginIO,
  )
import Data.Aeson
import Data.Maybe (fromJust, mapMaybe)
import GHC.Generics
import Models.AdminValidate (AdminV)
import Models.DBUser (DBUser)
import Models.User
  ( User (..),
    showAll,
    showUserAPI,
    stringToUser,
    writeUserOnFile,
  )
import Network.HTTP.Types (hContentType)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Util.Database.DBFunctions (deleteFromTableWhereAppDB)
import Util.Database.Functions.UsersDBFunctions (deleteFromUsersAppDB, deleteFromUsersWhereAppDB, insertAllIntoUsersAppDB, selectFromUsersWhereAppDB, updateInUsersWhereAppDB)
import Util.ScreenCleaner (start)
import Util.Validate
  ( belongsToList,
    handleValidationServer,
    userEmailValidation,
    userEnrollmentValidation,
    userNameValidation,
    userPasswordValidation,
    userUniversityValidation,
  )
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Util.Database.Functions.ValidationDBFunctions (deleteFromValidationsWhereAppDB)

newtype MyData = MyData {value :: String} deriving (Generic, FromJSON)

newtype IntegerData = IntegerData {integerValue :: Integer} deriving (Generic, FromJSON)

data LogInfo = LogInfo {email :: String, password :: String} deriving (Eq, Show, Generic, FromJSON)

data RegisterInfo = RegisterInfo {u_type :: String, user :: User} deriving (Eq, Show, Generic, FromJSON)

data ChangeData = ChangeData {field :: String, newValue :: String, match :: String, matchValue :: String} deriving (Eq, Show, Generic, FromJSON)

data GetUserFieldData = GetUserFieldData {unique_key_name :: Maybe String, unique_key :: Maybe String, attribute :: Maybe String} deriving (Eq, Show, Generic, FromJSON)

type API =
  "api"
    :> "users"
    :> ( "users" :> Get '[JSON] [User]
           :<|> "usersDB" :> Get '[JSON] [DBUser]
           :<|> "getIdsValidated" :> Get '[JSON] [AdminV]
           :<|> "validateName" :> ReqBody '[JSON] MyData :> Post '[JSON] String
           :<|> "validateUniversity" :> ReqBody '[JSON] MyData :> Post '[JSON] String
           :<|> "validateEmail" :> ReqBody '[JSON] MyData :> Post '[JSON] String
           :<|> "validateEnrollment" :> ReqBody '[JSON] MyData :> Post '[JSON] String
           :<|> "validatePassword" :> ReqBody '[JSON] MyData :> Post '[JSON] String
           :<|> "validateLogin" :> ReqBody '[JSON] LogInfo :> Post '[JSON] Bool
           :<|> "validateUser" :> ReqBody '[JSON] IntegerData :> Post '[JSON] NoContent
           :<|> "unvalidateUser" :> ReqBody '[JSON] MyData :> Post '[JSON] NoContent
           :<|> "register" :> ReqBody '[JSON] RegisterInfo :> Post '[JSON] String
           :<|> "isRegistered" :> ReqBody '[JSON] MyData :> Post '[JSON] Bool
           :<|> "showUser" :> ReqBody '[JSON] MyData :> Post '[JSON] String
           :<|> "showAllUsers" :> Get '[JSON] String
           :<|> "deleteUser" :> ReqBody '[JSON] MyData :> Post '[JSON] String
           :<|> "updateAny" :> ReqBody '[JSON] ChangeData :> Post '[JSON] String
           :<|> ( "getAny"
                    :> QueryParam "unique_key_name" String
                    :> QueryParam "unique_key" String
                    :> QueryParam "attribute" String
                    :> Get '[JSON] String
                )
       )

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: (HasServer (api01 :: *) '[]) => Proxy api01 -> Server api01 -> Application
app api ser = corsMiddleware $ serve api ser
  where
    corsMiddleware = cors (const $ Just simpleCorsResourcePolicy {corsRequestHeaders = [hContentType]})

userAPI :: Proxy API
userAPI = Proxy

superServer :: Server API
superServer =
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
unvalidateUser myData = liftIO (unvalidateUserAPI (value myData)) >> return NoContent

register :: RegisterInfo -> Handler String
register registerInfo = do
  isUserRegistered <- isRegistered (MyData {value = userEmail (user registerInfo)})
  if isUserRegistered
    then return "Failure"
    else do
      liftIO $ registerIn registerInfo
  where
    registerIn registerInfo
      | u_type registerInfo == "Professor" = liftIO $ registerUserAPI (user registerInfo) >> return "Success"
      | otherwise = liftIO $ registerStudentAPI (user registerInfo) >> return "Success"

isRegistered :: MyData -> Handler Bool
isRegistered emailData = do
  let email = value emailData
  allUsers <- liftIO getUsers
  return $ email `elem` map userEmail allUsers

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
    liftIO $ deleteFromValidationsWhereAppDB [("user_id", "=", value mydata)]
    liftIO $ deleteFromUsersWhereAppDB [("id", "=", value mydata)] 
    return "Success"
  else 
    return "You can't delete yourself, cuz you're the ADMIN"

updateAny :: ChangeData -> Handler String
updateAny mydata = do
  liftIO $ updateInUsersWhereAppDB [(field mydata, newValue mydata)] [(match mydata, "=", matchValue mydata)]
  showUser (MyData {value = matchValue mydata})

newtype RandomData = RandomData {userType' :: String} deriving (Show, Read, Eq, Generic)
instance FromRow RandomData
instance ToRow RandomData

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

serveOn :: IO ()
serveOn = start >> run 8081 (app userAPI superServer)
