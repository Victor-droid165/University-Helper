module Controllers.Users.UserController    
    ( userRegister
    , userLogin
    ) where

import Models.User
import Controllers.Users.AdministratorController ( administratorOptions )
import Controllers.Users.StudentController ( studentOptions )
import Controllers.Users.TeacherController ( teacherOptions )
import TerminalUI.Users.User ( registerUI, loginUI )

userRegister :: IO ()
userRegister = do
    (userType, userName, userUniversity, userEnrollment, userEmail, userPassword) <- registerUI
    let newUser = User { userType = userType
                       , userName = userName
                       , userUniversity = userUniversity
                       , userEnrollment = userEnrollment
                       , userEmail = userEmail
                       , userPassword = userPassword }

    writeUserOnFile "data/users.txt" newUser
    putStrLn "Usuário registrado com sucesso!"


userLogin :: IO ()
userLogin = do

    (userEmail, userPassword) <- loginUI

    maybeUser <- authenticateUser userEmail userPassword
    case maybeUser of
        Just user -> do
            writeUserOnFile "data/session.txt" user
            case userType user of
                "administrator" -> administratorOptions
                "student" -> studentOptions
                "teacher" -> teacherOptions
            putStrLn "Logged in successfully!"
        Nothing -> putStrLn "Invalid email or password."