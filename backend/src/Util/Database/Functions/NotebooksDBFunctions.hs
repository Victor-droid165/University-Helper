module Util.Database.Functions.NotebooksDBFunctions
  ( selectAllFromNotebooksWhereAppDB,
    selectAllFromNotebooksAppDB,
    insertAllIntoNotebooksAppDB,
    updateInNotebooksWhereAppDB,
    updateInNotebooksAppDB,
    selectFromNotebooksWhereAppDB,
    selectFromNotebooksAppDB,
    deleteFromNotebooksWhereAppDB,
    deleteFromNotebooksAppDB,
    updateAllInNotebooksAppDB,
    updateAllInNotebooksWhereAppDB,
  )
where

import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.ToField (ToField)
import Models.DB.DBNotebook (DBNotebook)
import Util.Database.DBFunctions (deleteFromTableAppDB, deleteFromTableWhereAppDB, insertAllIntoTableAppDB, selectFromTableAppDB, selectFromTableWhereAppDB, updateInTableAppDB, updateInTableWhereAppDB)

selectFromNotebooksWhereAppDB :: (FromRow a, ToField b) => [String] -> [(String, String, b)] -> IO [a]
selectFromNotebooksWhereAppDB = selectFromTableWhereAppDB "notebooks"

selectFromNotebooksAppDB :: (FromRow a) => [String] -> IO [a]
selectFromNotebooksAppDB = selectFromTableAppDB "notebooks"

selectAllFromNotebooksWhereAppDB :: (ToField b) => [(String, String, b)] -> IO [DBNotebook]
selectAllFromNotebooksWhereAppDB = selectFromTableWhereAppDB "notebooks" ["*"]

selectAllFromNotebooksAppDB :: IO [DBNotebook]
selectAllFromNotebooksAppDB = selectFromTableAppDB "notebooks" ["*"]

insertAllIntoNotebooksAppDB :: (ToField a) => [a] -> IO ()
insertAllIntoNotebooksAppDB = insertAllIntoTableAppDB "notebooks"

updateAllInNotebooksAppDB :: [String] -> IO ()
updateAllInNotebooksAppDB newValues = updateAllInNotebooksWhereAppDB newValues ([] :: [(String, String, String)])

updateAllInNotebooksWhereAppDB :: [String] -> [(String, String, String)] -> IO ()
updateAllInNotebooksWhereAppDB newValues = updateInNotebooksWhereAppDB $ zip ["id", "type", "visibility", "title", "subject", "content", "creator_id", "updated_at"] newValues

updateInNotebooksAppDB :: [(String, String)] -> IO ()
updateInNotebooksAppDB = updateInTableAppDB "notebooks"

updateInNotebooksWhereAppDB :: [(String, String)] -> [(String, String, String)] -> IO ()
updateInNotebooksWhereAppDB = updateInTableWhereAppDB "notebooks"

deleteFromNotebooksAppDB :: IO ()
deleteFromNotebooksAppDB = deleteFromTableAppDB "notebooks"

deleteFromNotebooksWhereAppDB :: (ToField b) => [(String, String, b)] -> IO ()
deleteFromNotebooksWhereAppDB = deleteFromTableWhereAppDB "notebooks"