module Util.Server.Notebooks.APIFunctions
  ( notebooksAPIFunctions,
  )
where

import Control.Monad.IO.Class (liftIO)
import Models.Notebook (Notebook, fromDBNotebook)
import Repositories.NotebookRepository (createNotebookInDB, removeNotebookFromDB, removeNotebookFromDBById)
import Servant
import Util.Database.Functions.NotebooksDBFunctions (selectAllFromNotebooksWhereAppDB)
import Util.Server.Notebooks.APIRoutes (NotebooksAPI)

notebooksAPIFunctions :: Server NotebooksAPI
notebooksAPIFunctions =
  notebooks
    :<|> removeNotebook
    :<|> removeNotebookByID
    :<|> registerNotebook

notebooks :: String -> Handler [Notebook]
notebooks id' = do
  notebookList <- liftIO $ selectAllFromNotebooksWhereAppDB [("creator_id", "=", id')]
  liftIO $ mapM fromDBNotebook notebookList

removeNotebook :: Notebook -> Handler String
removeNotebook notebook = liftIO $ removeNotebookFromDB notebook >> return "Removed"

removeNotebookByID :: String -> Handler String
removeNotebookByID id' = liftIO $ removeNotebookFromDBById id' >> return "Removed"

registerNotebook :: Notebook -> Handler String
registerNotebook notebook = liftIO $ createNotebookInDB notebook >> return "Created"
