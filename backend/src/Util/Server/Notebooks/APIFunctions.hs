module Util.Server.Notebooks.APIFunctions
  ( notebooksAPIFunctions,
  )
where

import Servant
import Util.Server.Notebooks.APIRoutes (NotebooksAPI)
import Util.Server.Users.APIDatas (MyData (..))
import Models.Notebook (Notebook, fromDBNotebook)
import Util.Database.Functions.NotebooksDBFunctions (selectAllFromNotebooksWhereAppDB)
import Control.Monad.IO.Class (liftIO)
import Repositories.NotebookRepository (removeNotebookFromDB, removeNotebookFromDBById, createNotebookInDB)

notebooksAPIFunctions :: Server NotebooksAPI
notebooksAPIFunctions =     notebooks
                  :<|>  removeNotebook
                  :<|>  removeNotebookByID 
                  :<|>  registerNotebook

notebooks :: MyData -> Handler [Notebook]
notebooks id = do
  notebookList <- liftIO $ selectAllFromNotebooksWhereAppDB [("creator_id", "=", value id)]
  liftIO $ mapM fromDBNotebook notebookList

removeNotebook :: Notebook -> Handler String
removeNotebook notebook = liftIO $ removeNotebookFromDB notebook >> return "Removed"

removeNotebookByID :: String -> Handler String
removeNotebookByID id = liftIO $ removeNotebookFromDBById id >> return "Removed"

registerNotebook :: Notebook -> Handler String
registerNotebook notebook = liftIO $ createNotebookInDB notebook >> return "Created"





