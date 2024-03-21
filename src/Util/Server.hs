{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}

module Util.Server (
  serveOn
) where

import Models.User hiding (userPassword, userEnrollment, userUniversity, userName, userEmail) 
import GHC.Generics
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Network.Wai.Middleware.Cors
import Network.HTTP.Types (hContentType)
import Controllers.Users.UserController (getUsers)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, object, (.=))
import Util.Validate (userNameValidation, handleValidationServer, userRegisterEmailValidation, userUniversityValidation, userEnrollmentValidation, userPasswordValidation)


data MyData = MyData { value :: String} deriving (Generic, FromJSON)

type API =    "users" :> Get '[JSON] [User]
              :<|> "register" :> ReqBody '[JSON] User :> Post '[JSON] NoContent
              :<|> "userName" :> ReqBody '[JSON] MyData :> Post '[JSON] String
              :<|> "userUniversity" :> ReqBody '[JSON] MyData :> Post '[JSON] String
              :<|> "userEmail" :> ReqBody '[JSON] MyData :> Post '[JSON] String
              :<|> "userEnrollment" :> ReqBody '[JSON] MyData :> Post '[JSON] String
              :<|> "userPassword" :> ReqBody '[JSON] MyData :> Post '[JSON] String

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
                :<|> userEmail
                :<|> userEnrollment
                :<|> userPassword

userEmail :: MyData -> Handler String
userEmail myData = return $ handleValidationServer (userRegisterEmailValidation (value myData))

userName :: MyData -> Handler String
userName myData = return $ handleValidationServer (userNameValidation (value myData))

userUniversity :: MyData -> Handler String
userUniversity myData = return $ handleValidationServer (userUniversityValidation (value myData))

userEnrollment :: MyData -> Handler String
userEnrollment myData = return $ handleValidationServer (userEnrollmentValidation (value myData))

userPassword :: MyData -> Handler String
userPassword myData = return $ handleValidationServer (userPasswordValidation (value myData))

users :: Handler [User]
users = liftIO getUsers

register :: User -> Handler NoContent
register user = liftIO (writeUserOnFile "data/users.txt" user) >> return NoContent

serveOn :: IO ()
serveOn = run 8081 (app userAPI superServer)
