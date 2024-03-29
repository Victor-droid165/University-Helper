{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Util.Server.Users.APIRoutes (UsersAPI) where

import Models.AdminValidate (AdminV)
import Models.DBUser (DBUser (..))
import Models.User
  ( User (..),
  )
import Servant
import Util.Server.Users.APIDatas

type UsersAPI =
  "usersG" :> Get '[JSON] [User]
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