module Controllers.Users.AdministratorController
    ( administratorOptions
    ) where

import Models.User
import TerminalUI.Users.User (registerUI, invalidOption, typeEnrollment)
import TerminalUI.Users.Administrator (userRegister, selectAction)
import Data.Maybe (mapMaybe)
import System.Directory

chooseOption :: Char -> IO ()
chooseOption choice
    | choice == '1' = userRegisterADMIN
    | choice == '2' = removeUser
    -- | choice == '3' = updateUser
    -- | choice == '4' = validateUser
    | otherwise = do
        invalidOption
        administratorOptions

userRegisterADMIN :: IO ()
userRegisterADMIN = do
    (userType, userName, userUniversity, userEnrollment, userEmail, userPassword) <- userRegister

    let newUser = User { userType = userType
                       , userName = userName
                       , userUniversity = userUniversity
                       , userEnrollment = userEnrollment
                       , userEmail = userEmail
                       , userPassword = userPassword }

    writeUserOnFile "data/users.txt" newUser
    putStrLn "Usuário registrado com sucesso!"

removeUser :: IO()
removeUser = do
    user <- typeEnrollment
    content <- readFile "data/users.txt"
    let placeHolderUser = User { userType = ""
                       , userName = ""
                       , userUniversity = ""
                       , userEnrollment = user
                       , userEmail = ""
                       , userPassword = "" }
    let userList = mapMaybe stringToUser (lines content)
    let newUserList = remove placeHolderUser userList
    removeFile "data/users.txt"
    mapM_ (writeUserOnFile "data/users.txt") newUserList


remove :: User -> [User] -> [User]
remove _ [] = []
remove user (u:userList)    | userEnrollment user == userEnrollment u = remove user userList
                            | otherwise = u : remove user userList


administratorOptions :: IO ()
administratorOptions = do
    choice <- selectAction
    chooseOption choice
