module Repositories.NotebookRepository
  ( getNotebooksFromDB,
    removeNotebookFromDBById,
    removeNotebookFromDB,
    updateNotebookInDB,
    createNotebookInDB,
    countNotebooksFromDB,
    countNotebooksPrefixesFromDB,
    getDBNotebooksFromDB,
    getNotebooksFromDBWhere
  )
where

import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.ToField (ToField)
import Models.DBCountResult (DBCountResult (DBCountResult))
import Models.DBNotebook (DBNotebook)
import Models.Notebook (Notebook (..), fromDBNotebook)
import Util.Database.Functions.NotebooksDBFunctions (deleteFromNotebooksWhereAppDB, insertAllIntoNotebooksAppDB, selectAllFromNotebooksAppDB, selectAllFromNotebooksWhereAppDB, selectFromNotebooksAppDB, selectFromNotebooksWhereAppDB, updateAllInNotebooksWhereAppDB)


getNotebooksFromDB :: IO [IO Notebook]
getNotebooksFromDB = getNotebooksFromDBWhere ([] :: [(String, String, String)])

getDBNotebooksFromDB :: IO [DBNotebook]
getDBNotebooksFromDB = selectAllFromNotebooksAppDB

getNotebooksFromDBWhere :: ToField b => [(String, String, b)] -> IO [IO Notebook]
getNotebooksFromDBWhere conditions = map fromDBNotebook <$> selectAllFromNotebooksWhereAppDB conditions

countNotebooksFromDB :: IO Int
countNotebooksFromDB = do
  result <- selectFromNotebooksAppDB ["COUNT(*)"] :: IO [DBCountResult]
  evaluateResult (head result)
  where
    evaluateResult (DBCountResult count) = return count

countNotebooksPrefixesFromDB :: String -> IO Int
countNotebooksPrefixesFromDB notebookPrefix = do
  result <- selectFromNotebooksWhereAppDB ["COUNT(*)"] [("id", "LIKE", notebookPrefix ++ "%")] :: IO [DBCountResult]
  evaluateResult (head result)
  where
    evaluateResult (DBCountResult count) = return count

createNotebookInDB :: Notebook -> IO ()
createNotebookInDB notebook = do
  let newNotebookValues = [notebookId notebook, notebookType notebook, notebookName notebook]
  insertAllIntoNotebooksAppDB newNotebookValues

updateNotebookInDB :: Notebook -> IO ()
updateNotebookInDB notebook = do
  let newValues = [notebookId notebook, notebookType notebook, notebookName notebook]
  updateAllInNotebooksWhereAppDB newValues [("id", "=", notebookId notebook)]

removeNotebookFromDB :: Notebook -> IO ()
removeNotebookFromDB = removeNotebookFromDBById . notebookId

removeNotebookFromDBById :: String -> IO ()
removeNotebookFromDBById notebookId' = deleteFromNotebooksWhereAppDB [("id", "=", notebookId')]