module Util.Server.Notes.APIFunctions
  ( notesAPIFunctions,
  )
where

import Servant
import Util.Server.Notes.APIRoutes (NotesAPI)
import Util.Server.Users.APIDatas (MyData (..))
import Models.Note (Note, fromDBNote)
import Util.Database.Functions.NotesDBFunctions (selectAllFromNotesWhereAppDB, selectAllFromNotesAppDB)
import Control.Monad.IO.Class (liftIO)
import Repositories.NoteRepository (removeNoteFromDB, removeNoteFromDBById, createNoteInDB)
import Controllers.Notes.NoteController (getNoteById, getNotes, getNotesByUserId, removeNoteById, removeByNote, updateNote, getNextNoteId, getDBNotes)

notesAPIFunctions :: Server NotesAPI
notesAPIFunctions =     notes
                  :<|>  removeNote
                  :<|>  removeNoteByID
                  :<|>  registerNote
                  :<|>  updateANote
                  :<|>  getId
                  :<|>  listNotes

notes :: MyData -> Handler [Note]
notes id = liftIO $ getNotesByUserId (value id)

removeNote :: Note -> Handler String
removeNote note = liftIO $ removeByNote note >> return "Removed"

removeNoteByID :: String -> Handler String
removeNoteByID id = liftIO $ removeNoteById id >> return "Removed"

registerNote :: Note -> Handler String
registerNote note = liftIO $ createNoteInDB note >> return "Created"

updateANote :: Note -> Handler String
updateANote note = liftIO $ updateNote note >> return "Updated"

getId :: MyData -> Handler String
getId prefix = liftIO $ getNextNoteId (value prefix)

listNotes :: Handler [Note]
listNotes = do
    dbNotes <- liftIO getDBNotes
    liftIO $ mapM fromDBNote dbNotes