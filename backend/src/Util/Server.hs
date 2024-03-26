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
import Controllers.Users.AdministratorController (unvalidateUserAPI, validateUserAPI)
import Controllers.Users.UserController (getUser, getUsers, verifyLoginIO)
import Data.Aeson
import Data.Maybe (mapMaybe)
import GHC.Generics
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
import Util.ScreenCleaner (start)
import Controllers.Users.UserController (registerUserAPI)
import Util.Validate
  ( belongsToList,
    handleValidationServer,
    userEmailValidation,
    userEnrollmentValidation,
    userNameValidation,
    userPasswordValidation,
    userUniversityValidation,
  )
import Util.Database.Functions.UsersDBFunctions (insertAllIntoUsersAppDB)

newtype MyData = MyData {value :: String} deriving (Generic, FromJSON)

data LogInfo = LogInfo {email :: String, password :: String} deriving (Eq, Show, Generic, FromJSON)

type API =
  "api"
    :> "users"
    :> ( "users" :> Get '[JSON] [User]
           :<|> "validateName" :> ReqBody '[JSON] MyData :> Post '[JSON] String
           :<|> "validateUniversity" :> ReqBody '[JSON] MyData :> Post '[JSON] String
           :<|> "validateEmail" :> ReqBody '[JSON] MyData :> Post '[JSON] String
           :<|> "validateEnrollment" :> ReqBody '[JSON] MyData :> Post '[JSON] String
           :<|> "validatePassword" :> ReqBody '[JSON] MyData :> Post '[JSON] String
           :<|> "validateLogin" :> ReqBody '[JSON] LogInfo :> Post '[JSON] Bool
           :<|> "validateUser" :> ReqBody '[JSON] MyData :> Post '[JSON] NoContent
           :<|> "unvalidateUser" :> ReqBody '[JSON] MyData :> Post '[JSON] NoContent
           :<|> "register" :> ReqBody '[JSON] User :> Post '[JSON] String
           :<|> "isRegistered" :> ReqBody '[JSON] MyData :> Post '[JSON] Bool
           :<|> "showUser" :> ReqBody '[JSON] MyData :> Post '[JSON] String
           :<|> "showAllUsers" :> Get '[JSON] String
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

validateUser :: MyData -> Handler NoContent
validateUser myData = liftIO (validateUserAPI (value myData)) >> return NoContent

unvalidateUser :: MyData -> Handler NoContent
unvalidateUser myData = liftIO (unvalidateUserAPI (value myData)) >> return NoContent

register :: User -> Handler String
register user = do
  isUserRegistered <- isRegistered (MyData {value = userEmail user})
  if isUserRegistered
    then return "Failure"
    else do
      liftIO $ registerUserAPI user
      return "Success"

isRegistered :: MyData -> Handler Bool
isRegistered emailData = do
  let email = value emailData
  allUsers <- liftIO getUsers
  return $ email `elem` map userEmail allUsers

showUser :: MyData -> Handler String
showUser myData = do
  users <- liftIO getUsers
  return $ showUserAPI (getUser (value myData) users)

showAllUsers :: Handler String
showAllUsers = do
  users <- liftIO getUsers
  return $ showAll users

-- LOGIN AND REGISTER

users :: Handler [User]
users = liftIO getUsers

serveOn :: IO ()
serveOn = start >> run 8081 (app userAPI superServer)
