{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Util.Server.Users.APIRoutes (UsersAPI) where

import Models.AdminValidate (AdminV)
import Models.DB.DBUpdateValue (DBUpdateValue)
import Models.DB.DBUser (DBUser (..), UserLogInfo)
import Models.User
  ( User (..),
  )
import Servant

type UsersAPI =
  "users" :> Get '[JSON] [User]
    :<|> "usersDB" :> Get '[JSON] [DBUser]
    :<|> "getIdsValidated" :> Get '[JSON] [AdminV]
    :<|> "validateName" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "validateUniversity" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "validateEmail" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "validateEnrollment" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "validatePassword" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "validateLogin" :> ReqBody '[JSON] UserLogInfo :> Post '[JSON] Bool
    :<|> "validateUser" :> ReqBody '[JSON] Int :> Post '[JSON] NoContent
    :<|> "unvalidateUser" :> ReqBody '[JSON] String :> Post '[JSON] NoContent
    :<|> "register" :> ReqBody '[JSON] User :> Post '[JSON] String
    :<|> "isRegistered" :> ReqBody '[JSON] String :> Post '[JSON] Bool
    :<|> "showUser" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "showAllUsers" :> Get '[JSON] String
    :<|> "deleteUser" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "updateAny" :> ReqBody '[JSON] DBUpdateValue :> Post '[JSON] String
    :<|> ( "getAny"
             :> QueryParam "unique_key_name" String
             :> QueryParam "unique_key" String
             :> QueryParam "attribute" String
             :> Get '[JSON] String
         )
    :<|> ( "user"
             :> QueryParam "unique_key_name" String
             :> QueryParam "unique_key" String
             :> Get '[JSON] User
         )