module Repositories.NoteRepository
  ( getNotesFromDB,
    removeNoteFromDBById,
    removeNoteFromDB,
    updateNoteInDB,
    createNoteInDB,
    countNotesFromDB,
    countNotesPrefixesFromDB,
  )
where

import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.ToField (ToField)
import Models.DBCountResult (DBCountResult (DBCountResult))
import Models.DBNote (DBNote)
import Models.Note (Note (..), fromDBNote)
import Repositories.UserRepository (getUserField)
import Util.Database.Functions.NotesDBFunctions (deleteFromNotesWhereAppDB, insertAllIntoNotesAppDB, selectAllFromNotesAppDB, selectAllFromNotesWhereAppDB, selectFromNotesAppDB, selectFromNotesWhereAppDB, updateAllInNotesWhereAppDB)


getNotesFromDB :: IO [IO Note]
getNotesFromDB = map fromDBNote <$> selectAllFromNotesAppDB

getDBNotesFromDB :: IO [DBNote]
getDBNotesFromDB = selectAllFromNotesAppDB

countNotesFromDB :: IO Int
countNotesFromDB = do
  result <- selectFromNotesAppDB ["COUNT(*)"] :: IO [DBCountResult]
  evaluateResult (head result)
  where
    evaluateResult (DBCountResult count) = return count

countNotesPrefixesFromDB :: String -> IO Int
countNotesPrefixesFromDB notePrefix = do
  result <- selectFromNotesWhereAppDB ["COUNT(*)"] [("id", "LIKE", notePrefix ++ "%")] :: IO [DBCountResult]
  evaluateResult (head result)
  where
    evaluateResult (DBCountResult count) = return count

createNoteInDB :: Note -> IO ()
createNoteInDB note = do
  userId <- getUserField (creator note) "id"
  let newNoteValues = [noteId note, noteType note, visibility note, show $ title note, show $ subject note, content note, userId]
  insertAllIntoNotesAppDB newNoteValues

updateNoteInDB :: Note -> IO ()
updateNoteInDB note = do
  let newValues = [noteId note, noteType note, visibility note, show $ title note, show $ subject note, content note]
  updateAllInNotesWhereAppDB newValues [("id", "=", noteId note)]

removeNoteFromDB :: Note -> IO ()
removeNoteFromDB = removeNoteFromDBById . noteId

removeNoteFromDBById :: String -> IO ()
removeNoteFromDBById noteId = deleteFromNotesWhereAppDB [("id", "=", noteId)]