{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Util.Server (
  serveOn
) where

import Models.User 
import GHC.Generics
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Network.Wai.Middleware.Cors
import Network.HTTP.Types (hContentType)
import Controllers.Users.UserController (getUsers)
import Control.Monad.IO.Class (liftIO)


type API = "users" :> Get '[JSON] [User]
              :<|> "register" :> ReqBody '[JSON] User :> Post '[JSON] NoContent

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

users :: Handler [User]
users = liftIO getUsers

register :: User -> Handler NoContent
register user = liftIO (writeUserOnFile "data/users.txt" user) >> return NoContent

serveOn :: IO ()
serveOn = run 8081 (app userAPI superServer)
