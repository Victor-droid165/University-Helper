module Controllers.Notebooks.NotebookController
  ( getNextNotebookId,
    registerNotebook,
    getNotebooks,
    getDBNotebooks,
    getNotebookById,
    updateNotebook,
    removeNotebook,
    removeNotebookById,
  )
where

import Models.DBNotebook (DBNotebook)
import Models.Notebook
import Repositories.NotebookRepository (countNotebooksPrefixesFromDB, createNotebookInDB, getDBNotebooksFromDB, getNotebooksFromDB, getNotebooksFromDBWhere, removeNotebookFromDB, removeNotebookFromDBById, updateNotebookInDB)

getNextNotebookId :: String -> IO String
getNextNotebookId notebookPrefix = do
  maxCurId <- countNotebooksPrefixesFromDB notebookPrefix
  return (notebookPrefix ++ "-" ++ show (maxCurId + 1))

getNotebooks :: IO [Notebook]
getNotebooks = sequence =<< getNotebooksFromDB

getDBNotebooks :: IO [DBNotebook]
getDBNotebooks = getDBNotebooksFromDB

getNotebookById :: String -> IO Notebook
getNotebookById notebookId' = do
  notebooks <- sequence =<< getNotebooksFromDBWhere [("id", "=", notebookId')]
  return $ head notebooks

registerNotebook :: Notebook -> IO ()
registerNotebook = createNotebookInDB

updateNotebook :: Notebook -> IO ()
updateNotebook = updateNotebookInDB

removeNotebook :: Notebook -> IO ()
removeNotebook = removeNotebookFromDB

removeNotebookById :: String -> IO ()
removeNotebookById = removeNotebookFromDBById