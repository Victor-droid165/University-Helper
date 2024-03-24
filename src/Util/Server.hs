{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}

module Util.Server (
  serveOn
) where

import Models.User
    ( User(userEmail, userType), writeUserOnFile, stringToUser, verifyLoginIO, getUserList, showUserAPI, getUserValidateList, showAll)
import GHC.Generics
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Network.Wai.Middleware.Cors
import Network.HTTP.Types (hContentType)
import Controllers.Users.UserController (getUsers)
import Control.Monad.IO.Class (liftIO)
import Util.Validate (userNameValidation, handleValidationServer, userRegisterEmailValidation, userUniversityValidation, userEnrollmentValidation, userPasswordValidation, belongsToList)
import Data.Maybe (mapMaybe)
import Control.Monad (forM)
import Util.ScreenCleaner (start)
import Controllers.Users.UserController (getUser)
import Controllers.Users.AdministratorController (validateUserAPI, unvalidateUserAPI)


data MyData = MyData { value :: String} deriving (Generic, FromJSON)
data LogInfo = LogInfo { email :: String, password :: String} deriving (Eq, Show, Generic, FromJSON)

type API =    "users" :> Get '[JSON] [User]
              :<|> "register" :> ReqBody '[JSON] User :> Post '[JSON] NoContent
              :<|> "userName" :> ReqBody '[JSON] MyData :> Post '[JSON] String
              :<|> "userUniversity" :> ReqBody '[JSON] MyData :> Post '[JSON] String
              :<|> "userEmail" :> ReqBody '[JSON] MyData :> Post '[JSON] String
              :<|> "userEnrollment" :> ReqBody '[JSON] MyData :> Post '[JSON] String
              :<|> "userPassword" :> ReqBody '[JSON] MyData :> Post '[JSON] String
              :<|> "isRegistered" :> ReqBody '[JSON] MyData :> Post '[JSON] String
              :<|> "userLogin" :> ReqBody '[JSON] LogInfo :> Post '[JSON] Bool
              :<|> "validateUser" :> ReqBody '[JSON] MyData :> Post '[JSON] NoContent
              :<|> "unvalidateUser" :> ReqBody '[JSON] MyData :> Post '[JSON] NoContent
              :<|> "showUser" :> ReqBody '[JSON] MyData :> Post '[JSON] String
              :<|> "showToValidate" :> Get '[JSON] String
              :<|> "showAllUsers" :> Get '[JSON] String

instance ToJSON User
instance FromJSON User

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: (HasServer (api01 :: *) '[]) => Proxy api01 -> Server api01 -> Application
app api ser = corsMiddleware $ serve api ser
  where
    corsMiddleware = cors (const $ Just simpleCorsResourcePolicy { corsRequestHeaders = [hContentType] })

userAPI :: Proxy API
userAPI = Proxy

superServer :: Server API
superServer =   users
                :<|> register
                :<|> userName
                :<|> userUniversity
                :<|> userEmailVali
                :<|> userEnrollment
                :<|> userPassword
                :<|> isRegistered
                :<|> userLogin
                :<|> validateUser
                :<|> unvalidateUser
                :<|> showUser
                :<|> showToValidate
                :<|> showALlUsers

isRegistered :: MyData -> Handler String
isRegistered email = do
    result <- liftIO $ verify (value email)
    if result == "Success"
        then return $ "Success"
        else return $ "Failure"
  where
    verify email = do
        content1 <- readFile "data/users.txt"
        content2 <- readFile "data/toValidate.txt"
        let list = mapMaybe stringToUser (lines (content1 ++ content2))
        validEmails <- forM list (\user -> return (userEmail user))
        return $ handleValidationServer (belongsToList validEmails email)

-- ADMIN OPTIONS
validateUser :: MyData -> Handler NoContent
validateUser myData = liftIO (validateUserAPI (value myData)) >> return NoContent

unvalidateUser :: MyData -> Handler NoContent
unvalidateUser myData = liftIO (unvalidateUserAPI (value myData)) >> return NoContent

showUser :: MyData -> Handler String
showUser myData = do 
  users <- liftIO getUserList
  return $ showUserAPI (getUser (value myData) users)

showALlUsers :: Handler String
showALlUsers = do
  users <- liftIO getUserList
  return $ showAll users

showToValidate :: Handler String
showToValidate = do
  users <- liftIO getUserValidateList
  return $ showAll users

-- VALIDATION
userEmailVali :: MyData -> Handler String
userEmailVali myData = return $ handleValidationServer (userRegisterEmailValidation (value myData))

userName :: MyData -> Handler String
userName myData = return $ handleValidationServer (userNameValidation (value myData))

userUniversity :: MyData -> Handler String
userUniversity myData = return $ handleValidationServer (userUniversityValidation (value myData))

userEnrollment :: MyData -> Handler String
userEnrollment myData = return $ handleValidationServer (userEnrollmentValidation (value myData))

userPassword :: MyData -> Handler String
userPassword myData = return $ handleValidationServer (userPasswordValidation (value myData))

-- LOGIN AND REGISTER
userLogin :: LogInfo -> Handler Bool
userLogin logInfo = liftIO $ verifyLoginIO (email logInfo) (password logInfo)

users :: Handler [User]
users = liftIO getUsers

register :: User -> Handler NoContent
register user | userType user == "student" = liftIO (writeUserOnFile "data/users.txt" user) >> return NoContent
              | userType user == "teacher" = liftIO (writeUserOnFile "data/toValidate.txt" user) >> return NoContent
              | otherwise = return NoContent

serveOn :: IO ()
serveOn = start >> run 8081 (app userAPI superServer)
