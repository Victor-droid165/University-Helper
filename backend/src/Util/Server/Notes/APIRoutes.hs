{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Util.Server.Notes.APIRoutes
  ( NotesAPI,
  )
where

import Servant

type NotesAPI = "createNote" :> Get '[JSON] NoContent