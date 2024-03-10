module Controllers.Users.AdministratorController
    ( administratorOptions
    ) where

import Models.User
import TerminalUI.Users.User (registerUI, invalidOption, typeEnrollment)
import TerminalUI.Users.Administrator (userRegister, selectAction)
import Data.Maybe (mapMaybe)
import System.Directory
import Data.Foldable (find)
import Util.ScreenCleaner (screenCleaner)

chooseOption :: Char -> IO ()
chooseOption choice
    | choice == '1' = userRegisterADMIN
    | choice == '2' = removeUser
    -- | choice == '3' = updateUser
    | choice == '4' = validateUser
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

validateUser :: IO ()
validateUser = do
    screenCleaner
    content <- readFile "data/toValidate.txt"
    let userList = mapMaybe stringToUser (lines content)
    mapM_ showUser userList
    putStrLn "Digite a matrícula do usuário que deseja validar: "
    enroll <- getLine
    --
    let placeHolderUser = getUser enroll userList
    let newValidateList = remove enroll userList
    removeFile "data/toValidate.txt"
    mapM_ (writeUserOnFile "data/toValidate.txt") newValidateList
    writeUserOnFile "data/users.txt" placeHolderUser

    --
    screenCleaner
    putStrLn "O seguinte usuário foi validado com sucesso: "
    showUser placeHolderUser

getUser :: String -> [User] -> User
getUser _ [] = User {}
getUser enroll (u:userList)   | enroll == userEnrollment u = u
                              | otherwise = getUser enroll userList

removeUser :: IO()
removeUser = do
    enroll <- typeEnrollment
    content <- readFile "data/users.txt"
    let userList = mapMaybe stringToUser (lines content)
    let newUserList = remove enroll userList
    removeFile "data/users.txt"
    mapM_ (writeUserOnFile "data/users.txt") newUserList


remove :: String -> [User] -> [User]
remove _ [] = []
remove enroll (u:userList)    | enroll == userEnrollment u = remove enroll userList
                              | otherwise = u : remove enroll userList


administratorOptions :: IO ()
administratorOptions = do
    choice <- selectAction
    chooseOption choice
