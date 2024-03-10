module TerminalUI.Users.Administrator
    ( selectAction
    , userRegister
    ) where

import Models.User
import Util.ScreenCleaner ( screenCleaner )
import TerminalUI.Users.User (typeUserName, typeUniversity, typeEnrollment, typeUserEmail, typeUserPassword)

userRegister :: IO (String, String, String, String, String, String)
userRegister = do
    userName <- typeUserName
    screenCleaner

    userUniversity <- typeUniversity
    screenCleaner

    userEnrollment <- typeEnrollment
    screenCleaner

    userEmail <- typeUserEmail [ "Agora informe-nos o e-mail do usuário",
                                 "Digite o E-MAIL da pessoa que utilizará o sistema:"]

    userPassword <- typeUserPassword ["Digite a SENHA que a pessoa utilizará para o login:"]
    screenCleaner
    let userType = "administrator"
    return (userType, userName, userUniversity, userEnrollment, userEmail, userPassword)

selectAction :: IO Char 
selectAction = do
    mapM_ putStrLn ["Qual tipo de operação você gostaria de realizar no momento?",
                    "[1] CADASTRAR USUÁRIO",
                    "[2] REMOVER USUÁRIO DO SISTEMA",
                    "[3] ATUALIZAR INFORMAÇÕES DE UM USUÁRIO",
                    "[4] VALIDAR CADASTROS",
                    "Digite o NÚMERO correspondente a sua opção:"]
    option <- getLine
    let chosenOption = head option
    return chosenOption
