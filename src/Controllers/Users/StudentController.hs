module Controllers.Users.StudentController
    ( studentOptions
    ) where

import Models.User
import TerminalUI.Users.User (registerUI, invalidOption, typeEnrollment)
import TerminalUI.Users.Student (selectAction)
import Data.Maybe (mapMaybe)
import System.Directory
import Data.Foldable (find)
import Util.ScreenCleaner (screenCleaner, quitIO, forceQuit)

chooseOption :: Char -> IO ()
chooseOption choice
    | choice == '1' = autoRemove
    | choice == '.' = quitIO studentOptions
    | otherwise = do
        invalidOption
        studentOptions


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
    forceQuit studentOptions

studentOptions :: IO ()
studentOptions = do
    choice <- selectAction
    chooseOption choice