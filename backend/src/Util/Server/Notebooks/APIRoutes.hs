{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Util.Server.Notebooks.APIRoutes
  ( NotebooksAPI,
  )
where

import Servant
import Models.Notebook (Notebook)
import Util.Server.Users.APIDatas (MyData)

type NotebooksAPI = "notebooks" :> ReqBody '[JSON] MyData :> Post '[JSON] [Notebook] 
           :<|> "removeNotebook" :> ReqBody '[JSON] Notebook :> Post '[JSON] String
           :<|> "removeByID" :> ReqBody '[JSON] String :> Post '[JSON] String
           :<|> "registerNotebook" :> ReqBody '[JSON] Notebook :> Post '[JSON] String

