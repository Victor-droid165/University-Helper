module Util.Server.Notes.APIFunctions
  ( notesAPIFunctions,
  )
where

import Control.Monad.IO.Class (liftIO)
import Controllers.Notes.NoteController (getDBNotes, getNextNoteId, getNotesByUserId, removeByNote, removeNoteById, updateNote)
import Models.Note (Note, fromDBNote)
import Repositories.NoteRepository (createNoteInDB)
import Servant
import Util.Server.Notes.APIRoutes (NotesAPI)

notesAPIFunctions :: Server NotesAPI
notesAPIFunctions =
  notes
    :<|> removeNote
    :<|> removeNoteByID
    :<|> registerNote
    :<|> updateANote
    :<|> getId
    :<|> listAllNotes

notes :: String -> Handler [Note]
notes id' = liftIO $ getNotesByUserId id'

removeNote :: Note -> Handler String
removeNote note = liftIO $ removeByNote note >> return "Removed"

removeNoteByID :: String -> Handler String
removeNoteByID id' = liftIO $ removeNoteById id' >> return "Removed"

registerNote :: Note -> Handler String
registerNote note = liftIO $ createNoteInDB note >> return "Created"

updateANote :: Note -> Handler String
updateANote note = liftIO $ updateNote note >> return "Updated"

getId :: String -> Handler String
getId prefix = liftIO $ getNextNoteId prefix

listAllNotes :: Handler [Note]
listAllNotes = do
  dbNotes <- liftIO getDBNotes
  liftIO $ mapM fromDBNote dbNotes