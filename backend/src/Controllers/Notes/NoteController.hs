module Controllers.Notes.NoteController
  ( getNextNoteId,
    registerNote,
    getNotes,
    getDBNotes,
    getNoteById,
    updateNote,
    removeByNote,
    removeNoteById,
    getNotesByUserId
  )
where

import Models.DBNote (DBNote)
import Models.Note ( Note )
import Repositories.NoteRepository (countNotesPrefixesFromDB, createNoteInDB, getDBNotesFromDB, getNotesFromDB, getNotesFromDBWhere, removeNoteFromDB, removeNoteFromDBById, updateNoteInDB)

getNextNoteId :: String -> IO String
getNextNoteId notePrefix = do
  maxCurId <- countNotesPrefixesFromDB notePrefix
  return (notePrefix ++ "-" ++ show (maxCurId + 1))

getNotes :: IO [Note]
getNotes = sequence =<< getNotesFromDB

getNotesByUserId :: String -> IO [Note]
getNotesByUserId id = sequence =<< getNotesFromDBWhere [("user_id", "=", id)]

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

removeByNote :: Note -> IO ()
removeByNote = removeNoteFromDB

removeNoteById :: String -> IO ()
removeNoteById = removeNoteFromDBById