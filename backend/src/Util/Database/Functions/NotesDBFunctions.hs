module Util.Database.Functions.NotesDBFunctions
  ( selectAllFromNotesWhereAppDB,
    selectAllFromNotesAppDB,
    insertAllIntoNotesAppDB,
    updateInNotesWhereAppDB,
    updateInNotesAppDB,
    selectFromNotesWhereAppDB,
    selectFromNotesAppDB,
    deleteFromNotesWhereAppDB,
    deleteFromNotesAppDB,
    updateAllInNotesAppDB,
    updateAllInNotesWhereAppDB,
    selectFromNoteIdsWhereAppDB,
    selectAllFromNoteIdsWhereAppDB,
    updateInNoteIdsWhereAppDB,
    insertAllIntoWarningNotificationsAppDB,
    selectFromWarningNotificationsAppDB,
    selectFromWarningNotificationsWhereAppDB,
    selectAllFromWarningNotificationsWhereAppDB,
    selectAllFromWarningNotificationsAppDB,
  )
where

import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.ToField (ToField)
import Models.DB.DBNote (DBNote)
import Models.DB.DBWarningNotification (DBWarningNotification)
import Util.Database.DBFunctions (deleteFromTableAppDB, deleteFromTableWhereAppDB, insertAllIntoTableAppDB, selectAllFromTableWhereAppDB, selectFromTableAppDB, selectFromTableWhereAppDB, updateInTableAppDB, updateInTableWhereAppDB)

selectFromNoteIdsWhereAppDB :: (FromRow a, ToField b) => [String] -> [(String, String, b)] -> IO [a]
selectFromNoteIdsWhereAppDB = selectFromTableWhereAppDB "note_ids"

selectAllFromNoteIdsWhereAppDB :: (FromRow a, ToField b) => [(String, String, b)] -> IO [a]
selectAllFromNoteIdsWhereAppDB = selectAllFromTableWhereAppDB "note_ids"

selectFromNotesWhereAppDB :: (FromRow a, ToField b) => [String] -> [(String, String, b)] -> IO [a]
selectFromNotesWhereAppDB = selectFromTableWhereAppDB "notes"

selectFromNotesAppDB :: (FromRow a) => [String] -> IO [a]
selectFromNotesAppDB = selectFromTableAppDB "notes"

selectAllFromNotesWhereAppDB :: (ToField b) => [(String, String, b)] -> IO [DBNote]
selectAllFromNotesWhereAppDB = selectFromTableWhereAppDB "notes" ["*"]

selectAllFromNotesAppDB :: IO [DBNote]
selectAllFromNotesAppDB = selectFromTableAppDB "notes" ["*"]

selectFromWarningNotificationsWhereAppDB :: (FromRow a, ToField b) => [String] -> [(String, String, b)] -> IO [a]
selectFromWarningNotificationsWhereAppDB = selectFromTableWhereAppDB "user_warnings"

selectFromWarningNotificationsAppDB :: (FromRow a) => [String] -> IO [a]
selectFromWarningNotificationsAppDB = selectFromTableAppDB "user_warnings"

selectAllFromWarningNotificationsWhereAppDB :: (ToField b) => [(String, String, b)] -> IO [DBWarningNotification]
selectAllFromWarningNotificationsWhereAppDB = selectFromTableWhereAppDB "user_warnings" ["*"]

selectAllFromWarningNotificationsAppDB :: IO [DBWarningNotification]
selectAllFromWarningNotificationsAppDB = selectFromTableAppDB "user_warnings" ["*"]

insertAllIntoNotesAppDB :: (ToField a) => [a] -> IO ()
insertAllIntoNotesAppDB = insertAllIntoTableAppDB "notes"

insertAllIntoWarningNotificationsAppDB :: (ToField a) => [a] -> IO ()
insertAllIntoWarningNotificationsAppDB = insertAllIntoTableAppDB "user_warnings"

updateAllInNotesAppDB :: [String] -> IO ()
updateAllInNotesAppDB newValues = updateAllInNotesWhereAppDB newValues ([] :: [(String, String, String)])

updateAllInNotesWhereAppDB :: [String] -> [(String, String, String)] -> IO ()
updateAllInNotesWhereAppDB newValues = updateInNotesWhereAppDB $ zip ["id", "type", "visibility", "title", "subject", "content", "creator_id", "updated_at"] newValues

updateInNotesAppDB :: [(String, String)] -> IO ()
updateInNotesAppDB = updateInTableAppDB "notes"

updateInNotesWhereAppDB :: [(String, String)] -> [(String, String, String)] -> IO ()
updateInNotesWhereAppDB = updateInTableWhereAppDB "notes"

updateInNoteIdsWhereAppDB :: [(String, String)] -> [(String, String, String)] -> IO ()
updateInNoteIdsWhereAppDB = updateInTableWhereAppDB "note_ids"

deleteFromNotesAppDB :: IO ()
deleteFromNotesAppDB = deleteFromTableAppDB "notes"

deleteFromNotesWhereAppDB :: (ToField b) => [(String, String, b)] -> IO ()
deleteFromNotesWhereAppDB = deleteFromTableWhereAppDB "notes"