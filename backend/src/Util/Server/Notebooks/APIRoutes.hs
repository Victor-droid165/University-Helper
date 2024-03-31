{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Util.Server.Notebooks.APIRoutes
  ( NotebooksAPI,
  )
where

import Models.Notebook (Notebook)
import Servant

type NotebooksAPI =
  "notebooks" :> ReqBody '[JSON] String :> Post '[JSON] [Notebook]
    :<|> "removeNotebook" :> ReqBody '[JSON] Notebook :> Post '[JSON] String
    :<|> "removeByID" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "registerNotebook" :> ReqBody '[JSON] Notebook :> Post '[JSON] String
