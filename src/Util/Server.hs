{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Util.Server (
  serveOn
) where

import Models.User 
import GHC.Generics
import Data.Aeson
import Data.Time.Calendar
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Network.Wai.Middleware.Cors
import Network.HTTP.Types (hContentType)
import Controllers.Users.UserController (getUsers)
import Control.Monad.IO.Class (liftIO)

type UserAPI1 = "users" :> Get '[JSON] [User]

instance ToJSON User

users1 :: IO [User]
users1 = getUsers

userAPI :: Proxy UserAPI1
userAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = corsMiddleware $ serve userAPI server1
  where
    corsMiddleware = cors (const $ Just simpleCorsResourcePolicy { corsRequestHeaders = [hContentType] })

server1 :: Server UserAPI1
server1 = getUsersHandler

getUsersHandler :: Handler [User]
getUsersHandler = liftIO users1

serveOn :: IO ()
serveOn = run 8081 app1
