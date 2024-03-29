module Util.Server.Notes.APIFunctions
  ( notesAPIFunctions,
  )
where

import Servant
import Util.Server.Notes.APIRoutes (NotesAPI)
import Util.Server.Users.APIDatas (MyData (..))
import Models.Note (Note, fromDBNote)
import Util.Database.Functions.NotesDBFunctions (selectAllFromNotesWhereAppDB)
import Control.Monad.IO.Class (liftIO)
import Repositories.NoteRepository (removeNoteFromDB, removeNoteFromDBById, createNoteInDB)

notesAPIFunctions :: Server NotesAPI
notesAPIFunctions =     notes
                  :<|>  removeNote
                  :<|>  removeNoteByID 
                  :<|>  registerNote

notes :: MyData -> Handler [Note]
notes id = do
  noteList <- liftIO $ selectAllFromNotesWhereAppDB [("creator_id", "=", value id)]
  liftIO $ mapM fromDBNote noteList

removeNote :: Note -> Handler String
removeNote note = liftIO $ removeNoteFromDB note >> return "Removed"

removeNoteByID :: String -> Handler String
removeNoteByID id = liftIO $ removeNoteFromDBById id >> return "Removed"

registerNote :: Note -> Handler String
registerNote note = liftIO $ createNoteInDB note >> return "Created"





