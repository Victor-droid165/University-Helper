module Repositories.NoteRepository
  ( getNotesFromDB,
    removeNoteFromDBById,
    removeNoteFromDB,
    updateNoteInDB,
    createNoteInDB,
    getDBusersFromDB,
  )
where

import Models.Note (Note (..), fromDBNote)
import Util.Database.Functions.NotesDBFunctions (deleteFromNotesWhereAppDB, selectAllFromNotesAppDB, updateAllInNotesWhereAppDB, insertAllIntoNotesAppDB, selectAllFromNotesWhereAppDB)
import Models.DBNote (DBNote)
import Database.PostgreSQL.Simple.ToField (ToField)
import Repositories.UserRepository (getUserField)
import Data.Aeson (FromJSON)

instance FromJSON Note

getNotesFromDB :: IO [IO Note]
getNotesFromDB = map fromDBNote <$> selectAllFromNotesAppDB

getDBusersFromDB :: IO [DBNote]
getDBusersFromDB = selectAllFromNotesAppDB

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