module Controllers.Notebooks.NotebookController
  ( registerNotebook,
    getNotebooks,
    getDBNotebooks,
    getNotebookById,
    updateNotebook,
    removeNotebook,
    removeNotebookById,
  )
where

import Models.DB.DBNotebook (DBNotebook)
import Models.Notebook (Notebook)
import Repositories.NotebookRepository
  ( createNotebookInDB,
    getDBNotebooksFromDB,
    getNotebooksFromDB,
    getNotebooksFromDBWhere,
    removeNotebookFromDB,
    removeNotebookFromDBById,
    updateNotebookInDB,
  )

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