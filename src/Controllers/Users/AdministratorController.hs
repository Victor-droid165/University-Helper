module Controllers.Users.AdministratorController
    ( administratorOptions
    ) where

import Models.User
import TerminalUI.Users.User (registerUI, invalidOption, typeEnrollment)
import TerminalUI.Users.Administrator (userRegister, selectAction)
import Data.Maybe (mapMaybe)
import System.Directory
import Data.Foldable (find)
import Util.ScreenCleaner (screenCleaner, quitIO, forceQuit)

chooseOption :: Char -> IO ()
chooseOption choice
    | choice == '1' = userRegisterADMIN
    | choice == '2' = userRemove
    | choice == '3' = updateUser
    | choice == '4' = validateUser
    | choice == '.' = quitIO administratorOptions
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
    administratorOptions

updateUser :: IO ()
updateUser = do
    screenCleaner
    putStrLn "Digite a matrícula do Usuário que pretende alterar o tipo: "
    enroll <- getLine
    content <- readFile "data/users.txt"
    let userList = mapMaybe stringToUser (lines content)
    let user = getUser enroll userList
    
    screenCleaner
    newType <- selectType user 
    let newUser = setType newType user
    let newUserL = swapUser user newUser userList
    removeFile "data/users.txt"
    mapM_ (writeUserOnFile "data/users.txt") newUserL

    screenCleaner
    putStrLn "O seguinte usuário foi atualizado com sucesso: "
    displayUser newUser
    administratorOptions

        
updateOpt :: Char -> User -> IO String
updateOpt choice user   | choice == '1' = return "administrator"
                        | choice == '2' = return "teacher"
                        | choice == '3' = return "student"
                        | otherwise = do
                            invalidOption
                            selectType user

selectType :: User -> IO String
selectType user = do
    mapM_ putStrLn ["Defina um novo tipo para " ++ userName user ++ ":",
                    "[1] - Administrador",
                    "[2] - Professor",
                    "[3] - Aluno"]
    option <- getLine
    let chosenOption = head option
    updateOpt chosenOption user


swapUser :: User -> User -> [User] -> [User]
swapUser _ _ [] = []
swapUser old new (u:userL)  | old == u = new : swapUser old new userL 
                            | otherwise = u : swapUser old new userL


validateUser :: IO ()
validateUser = do
    screenCleaner
    content <- readFile "data/toValidate.txt"
    let userList = mapMaybe stringToUser (lines content)
    mapM_ displayUser userList
    putStrLn "Digite a matrícula do usuário que deseja validar: "
    enroll <- getLine
    --
    let placeHolderUser = getUser enroll userList
    let newValidateList = removeUser enroll userList
    removeFile "data/toValidate.txt"
    mapM_ (writeUserOnFile "data/toValidate.txt") newValidateList
    writeUserOnFile "data/users.txt" placeHolderUser

    --
    screenCleaner
    putStrLn "O seguinte usuário foi validado com sucesso: "
    displayUser placeHolderUser
    administratorOptions


getUser :: String -> [User] -> User
getUser _ [] = User {}
getUser enroll (u:userList)   | enroll == userEnrollment u = u
                              | otherwise = getUser enroll userList


userRemove :: IO()
userRemove = do
    enroll <- typeEnrollment ["Agora precisamos saber qual a matrícula do usuário",
                    "Digite o numero de MATRÍCULA da pessoa que você removerá do sistema(isso pode incluir você mesmo):"]
    content <- readFile "data/users.txt"

    loggedUser <- readFile "data/session.txt"
    let currentUser = stringToUser (head(lines loggedUser))
    let userLoggedEnrollment = maybe "" userEnrollment currentUser

    let userList = mapMaybe stringToUser (lines content)

    if enroll == userLoggedEnrollment then autoRemove else do
        let newUserList = removeUser enroll userList
        removeFile "data/users.txt"
        mapM_ (writeUserOnFile "data/users.txt") newUserList
        administratorOptions


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
    forceQuit administratorOptions


administratorOptions :: IO ()
administratorOptions = do
    choice <- selectAction
    chooseOption choice
