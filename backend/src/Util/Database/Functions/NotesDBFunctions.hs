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
  )
where

import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.ToField (ToField)
import Models.DBNote (DBNote)
import Util.Database.DBFunctions (deleteFromTableAppDB, deleteFromTableWhereAppDB, insertAllIntoTableAppDB, selectFromTableAppDB, selectFromTableWhereAppDB, updateInTableAppDB, updateInTableWhereAppDB)

selectFromNotesWhereAppDB :: (FromRow a, ToField b) => [String] -> [(String, String, b)] -> IO [a]
selectFromNotesWhereAppDB = selectFromTableWhereAppDB "notes"

selectFromNotesAppDB :: (FromRow a) => [String] -> IO [a]
selectFromNotesAppDB = selectFromTableAppDB "notes"

selectAllFromNotesWhereAppDB :: (ToField b) => [(String, String, b)] -> IO [DBNote]
selectAllFromNotesWhereAppDB = selectFromTableWhereAppDB "notes" ["*"]

selectAllFromNotesAppDB :: IO [DBNote]
selectAllFromNotesAppDB = selectFromTableAppDB "notes" ["*"]

insertAllIntoNotesAppDB :: (ToField a) => [a] -> IO ()
insertAllIntoNotesAppDB = insertAllIntoTableAppDB "notes"

updateAllInNotesAppDB :: [String] -> IO ()
updateAllInNotesAppDB newValues = updateAllInNotesWhereAppDB newValues ([] :: [(String, String, String)])

updateAllInNotesWhereAppDB :: [String] -> [(String, String, String)] -> IO ()
updateAllInNotesWhereAppDB newValues = updateInNotesWhereAppDB $ zip ["id", "type", "visibility", "title", "subject", "content", "creator_id", "updated_at"] newValues

updateInNotesAppDB :: [(String, String)] -> IO ()
updateInNotesAppDB = updateInTableAppDB "notes"

updateInNotesWhereAppDB :: [(String, String)] -> [(String, String, String)] -> IO ()
updateInNotesWhereAppDB = updateInTableWhereAppDB "notes"

deleteFromNotesAppDB :: IO ()
deleteFromNotesAppDB = deleteFromTableAppDB "notes"

deleteFromNotesWhereAppDB :: (ToField b) => [(String, String, b)] -> IO ()
deleteFromNotesWhereAppDB = deleteFromTableWhereAppDB "notes"