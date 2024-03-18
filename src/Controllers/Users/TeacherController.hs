module Controllers.Users.TeacherController
    ( teacherOptions
    ) where

import Models.User
import TerminalUI.Users.User (registerUI, typeEnrollment)
import TerminalUI.Users.Teacher (displayActionSelection)
import Data.Maybe (mapMaybe)
import System.Directory
import Data.Foldable (find)
import Util.ScreenCleaner (screenCleaner, quitIO, forceQuit)
import Lib (selectOption)

autoRemove :: IO()
autoRemove = do
    loggedUser <- readFile "data/session.txt"
    let userToRemove = stringToUser (head(lines loggedUser))
    let enrollment = maybe "" userEnrollment userToRemove

    content <- readFile "data/users.txt"
    let userList = mapMaybe stringToUser (lines content)
    let newUserList = removeUser enrollment userList

    removeFile "data/users.txt"
    mapM_ (writeUserOnFile "data/users.txt") newUserList
    forceQuit teacherOptions

teacherOptions :: IO ()
teacherOptions = do
    actionPrompts <- displayActionSelection
    selectOption $ zip actionPrompts [autoRemove, quitIO teacherOptions]