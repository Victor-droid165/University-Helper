module Controllers.Users.StudentController
  ( studentOptions,
  )
where

import Data.Foldable (find)
import Data.Maybe (mapMaybe)
import Lib (selectOption)
import Models.User
import System.Directory
import TerminalUI.Users.Student (displayActionSelection)
import TerminalUI.Users.User (invalidOption, registerUI, typeEnrollment)
import Util.ScreenCleaner (forceQuit, quitIO, screenCleaner)

autoRemove :: IO ()
autoRemove = do
  loggedUser <- readFile "data/session.txt"
  let userToRemove = stringToUser (head (lines loggedUser))
  let enrollment = maybe "" userEnrollment userToRemove

  content <- readFile "data/users.txt"
  let userList = mapMaybe stringToUser (lines content)
  let newUserList = removeUser enrollment userList

  removeFile "data/users.txt"
  mapM_ (writeUserOnFile "data/users.txt") newUserList
  forceQuit studentOptions

studentOptions :: IO ()
studentOptions = do
  actionPrompts <- displayActionSelection
  selectOption $ zip actionPrompts [autoRemove, quitIO studentOptions]