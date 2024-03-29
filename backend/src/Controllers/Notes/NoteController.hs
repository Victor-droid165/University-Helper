module Controllers.Notes.NoteController
  ( getNextNoteId,
    registerNote,
    getNotes,
    getDBNotes,
    getNoteById,
    updateNote,
    removeNote,
    removeNoteById,
  )
where

import Models.DBNote (DBNote)
import Models.Note
import Repositories.NoteRepository (countNotesPrefixesFromDB, createNoteInDB, getDBNotesFromDB, getNotesFromDB, getNotesFromDBWhere, removeNoteFromDB, removeNoteFromDBById, updateNoteInDB)

getNextNoteId :: String -> IO String
getNextNoteId notePrefix = do
  maxCurId <- countNotesPrefixesFromDB notePrefix
  return (notePrefix ++ "-" ++ show (maxCurId + 1))

getNotes :: IO [Note]
getNotes = sequence =<< getNotesFromDB

getDBNotes :: IO [DBNote]
getDBNotes = getDBNotesFromDB

getNoteById :: String -> IO Note
getNoteById noteId' = do
  notes <- sequence =<< getNotesFromDBWhere [("id", "=", noteId')]
  return $ head notes

registerNote :: Note -> IO ()
registerNote = createNoteInDB

updateNote :: Note -> IO ()
updateNote = updateNoteInDB

removeNote :: Note -> IO ()
removeNote = removeNoteFromDB

removeNoteById :: String -> IO ()
removeNoteById = removeNoteFromDBById