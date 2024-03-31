module Repositories.NoteRepository
  ( getNotesFromDB,
    removeNoteFromDBById,
    removeNoteFromDB,
    updateNoteInDB,
    createNoteInDB,
    countNotesFromDB,
    countNotesPrefixesFromDB,
    getDBNotesFromDB,
    getNotesFromDBWhere,
    getNoteIdFromDBWhere,
    updateNoteIdInDB,
  )
where

import Database.PostgreSQL.Simple.ToField (ToField)
import Models.DB.DBCountResult (DBCountResult (DBCountResult))
import Models.DB.DBNote (DBNote (..))
import Models.DB.DBNoteId (DBNoteId (dbIdNum, dbPrefix))
import Models.Note (Note (..), fromDBNote)
import Models.WrapperTypes.IntWrapper (IntWrapper, extractInt)
import Repositories.UserRepository (getUserField)
import Util.Database.Functions.NotesDBFunctions (deleteFromNotesWhereAppDB, insertAllIntoNotesAppDB, selectAllFromNoteIdsWhereAppDB, selectAllFromNotesAppDB, selectAllFromNotesWhereAppDB, selectFromNotesAppDB, selectFromNotesWhereAppDB, updateAllInNotesWhereAppDB, updateInNoteIdsWhereAppDB)

getNoteIdFromDBWhere :: (ToField b) => [(String, String, b)] -> IO [DBNoteId]
getNoteIdFromDBWhere = selectAllFromNoteIdsWhereAppDB

getNotesFromDB :: IO [IO Note]
getNotesFromDB = getNotesFromDBWhere ([] :: [(String, String, String)])

getDBNotesFromDB :: IO [DBNote]
getDBNotesFromDB = selectAllFromNotesAppDB

getNotesFromDBWhere :: (ToField b) => [(String, String, b)] -> IO [IO Note]
getNotesFromDBWhere conditions = map fromDBNote <$> selectAllFromNotesWhereAppDB conditions

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
  userId <- getUserField (creator note) "id" :: IO IntWrapper
  let newNoteValues = [noteId note, noteType note, visibility note, show $ title note, show $ subject note, content note, show $ extractInt userId]
  insertAllIntoNotesAppDB newNoteValues

updateNoteInDB :: Note -> IO ()
updateNoteInDB note = do
  let newValues = [noteId note, noteType note, visibility note, show $ title note, show $ subject note, content note]
  updateAllInNotesWhereAppDB newValues [("id", "=", noteId note)]

updateNoteIdInDB :: DBNoteId -> IO ()
updateNoteIdInDB dbNoteId' = do
  updateInNoteIdsWhereAppDB [("id_num", (show . (+ 1) . dbIdNum) dbNoteId')] [("prefix", "=", dbPrefix dbNoteId')]

removeNoteFromDB :: Note -> IO ()
removeNoteFromDB = removeNoteFromDBById . noteId

removeNoteFromDBById :: String -> IO ()
removeNoteFromDBById noteId' = deleteFromNotesWhereAppDB [("id", "=", noteId')]