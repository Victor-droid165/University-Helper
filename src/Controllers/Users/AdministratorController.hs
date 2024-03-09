module Controllers.Users.AdministratorController
    ( administratorOptions
    ) where

import Models.User
import TerminalUI.Users.User (registerUI, invalidOption)
import TerminalUI.Users.Administrator (userRegister, selectAction)

chooseOption :: Char -> IO ()
chooseOption choice
    | choice == '1' = userRegisterByAdministrator
    -- | choice == '2' = removeUser
    -- | choice == '3' = updateUser
    -- | choice == '4' = validateUser
    | otherwise = do
        invalidOption
        administratorOptions

userRegisterByAdministrator :: IO ()
userRegisterByAdministrator = do
    (userType, userName, userUniversity, userEnrollment, userEmail, userPassword) <- userRegister

    let newUser = User { userType = userType
                       , userName = userName
                       , userUniversity = userUniversity
                       , userEnrollment = userEnrollment
                       , userEmail = userEmail
                       , userPassword = userPassword }

    writeUserOnFile "data/users.txt" newUser
    putStrLn "Usuário registrado com sucesso!"

administratorOptions :: IO ()
administratorOptions = do
    choice <- selectAction
    chooseOption choice
