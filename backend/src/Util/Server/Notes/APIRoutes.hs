{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Util.Server.Notes.APIRoutes
  ( NotesAPI,
  )
where

import Models.DB.DBWarningNotification (DBWarningNotification)
import Models.Note (Note)
import Servant

type NotesAPI =
  "notes" :> ReqBody '[JSON] String :> Post '[JSON] [Note]
    :<|> "removeNote" :> ReqBody '[JSON] Note :> Post '[JSON] String
    :<|> "removeByID" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "registerNote" :> ReqBody '[JSON] Note :> Post '[JSON] String
    :<|> "updateANote" :> ReqBody '[JSON] Note :> Post '[JSON] String
    :<|> "getId" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "listAllNotes" :> Get '[JSON] [Note]
    :<|> "notifyUser" :> ReqBody '[JSON] DBWarningNotification :> Post '[JSON] Bool
    :<|> ( "userNotifications"
             :> QueryParam "dbUserId" Int
             :> Get '[JSON] [Note]
         )
    