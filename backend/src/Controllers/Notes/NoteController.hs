module Controllers.Notes.NotesController (
    registerNote
) where

import Repositories.NoteRepository
import Models.Note
import Data.Aeson (FromJSON)

registerNote :: Note -> IO()
registerNote = createNoteInDB

