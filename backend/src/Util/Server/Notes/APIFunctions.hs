module Util.Server.Notes.APIFunctions
  ( notesAPIFunctions,
  )
where

import Servant
import Util.Server.Notes.APIRoutes (NotesAPI)

notesAPIFunctions :: Server NotesAPI
notesAPIFunctions = notes

notes :: Handler NoContent
notes = return NoContent