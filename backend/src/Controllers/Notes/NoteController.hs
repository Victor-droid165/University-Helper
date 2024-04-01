module Controllers.Notes.NoteController
  ( getNextNoteId,
    registerNote,
    getNotes,
    getDBNotes,
    getNoteById,
    updateNote,
    removeByNote,
    removeNoteById,
    getNotesByUserId,
  )
where

import Models.DB.DBNote (DBNote)
import Models.DB.DBNoteId (DBNoteId (..))
import Models.Note (Note)
import Repositories.NoteRepository
  ( createNoteInDB,
    getDBNotesFromDB,
    getNoteIdFromDBWhere,
    getNotesFromDB,
    getNotesFromDBWhere,
    removeNoteFromDB,
    removeNoteFromDBById,
    updateNoteIdInDB,
    updateNoteInDB,
  )

getNextNoteId :: String -> IO String
getNextNoteId notePrefix = do
  nextDBNoteIds <- getNoteIdFromDBWhere [("prefix", "=", notePrefix)] :: IO [DBNoteId]
  let nextDBNoteId = head nextDBNoteIds
      nextNoteId = dbPrefix nextDBNoteId ++ "-" ++ show (dbIdNum nextDBNoteId)
  updateNoteIdInDB nextDBNoteId
  return nextNoteId

getNotes :: IO [Note]
getNotes = sequence =<< getNotesFromDB

getNotesByUserId :: String -> IO [Note]
getNotesByUserId id' = sequence =<< getNotesFromDBWhere [("creator_id", "=", id')]

getDBNotes :: IO [DBNote]
getDBNotes = getDBNotesFromDB

getNoteById :: String -> IO Note
getNoteById noteId' = do
  note <- sequence =<< getNotesFromDBWhere [("id", "=", noteId')]
  return $ head note

registerNote :: Note -> IO ()
registerNote = createNoteInDB

updateNote :: Note -> IO ()
updateNote = updateNoteInDB

removeByNote :: Note -> IO ()
removeByNote = removeNoteFromDB

removeNoteById :: String -> IO ()
removeNoteById = removeNoteFromDBById