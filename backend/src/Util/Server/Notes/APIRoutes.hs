{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Util.Server.Notes.APIRoutes
  ( NotesAPI,
  )
where

import Servant
import Models.Note (Note)
import Util.Server.Users.APIDatas (MyData)

type NotesAPI = "notes" :> ReqBody '[JSON] MyData :> Post '[JSON] [Note] 
           :<|> "removeNote" :> ReqBody '[JSON] Note :> Post '[JSON] String
           :<|> "removeByID" :> ReqBody '[JSON] String :> Post '[JSON] String
           :<|> "registerNote" :> ReqBody '[JSON] Note :> Post '[JSON] String
           :<|> "updateANote" :> ReqBody '[JSON] Note :> Post '[JSON] String
           :<|> "getId" :> ReqBody '[JSON] MyData :> Post '[JSON] String