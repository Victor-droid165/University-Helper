{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Util.Server.API
  ( serveOn,
  )
where

import Network.HTTP.Types (hContentType)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Util.ScreenCleaner (start)
import Util.Server.Notes.APIFunctions (notesAPIFunctions)
import Util.Server.Notes.APIRoutes (NotesAPI)
import Util.Server.Users.APIFunctions (usersAPIFunctions)
import Util.Server.Users.APIRoutes (UsersAPI)

type API = "api" :>
    ("users" :> UsersAPI
    :<|> "notes" :> NotesAPI)

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: (HasServer (api01 :: *) '[]) => Proxy api01 -> Server api01 -> Application
app api ser = corsMiddleware $ serve api ser
  where
    corsMiddleware = cors (const $ Just simpleCorsResourcePolicy {corsRequestHeaders = [hContentType]})

proxyAPI :: Proxy API
proxyAPI = Proxy

serveOn :: IO ()
serveOn = start >> run 8081 (app proxyAPI serverS)

serverS :: Server API
serverS = usersAPIFunctions :<|> notesAPIFunctions