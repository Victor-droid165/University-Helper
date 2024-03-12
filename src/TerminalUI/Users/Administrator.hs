module TerminalUI.Users.Administrator
    ( selectAction
    , userRegister
    ) where

import Models.User
import Util.ScreenCleaner ( screenCleaner )
import TerminalUI.Users.User (typeUserName, typeUniversity, typeEnrollment, typeUserEmail, typeUserPassword)

invalidOption :: IO ()
invalidOption = do
    mapM_ putStrLn ["Opção Inválida. Tente Novamente.",
                    "Se deseja escolher a opção '[X] - Opção', digite: X\n"]

chooseOption :: Char -> IO String
chooseOption choice
    | choice == '1' = return "administrator"
    | choice == '2' = return "teacher"
    | choice == '3' = return "student"
    | otherwise = do
        invalidOption
        selectAccountType

selectAccountType :: IO String
selectAccountType = do
    mapM_ putStrLn ["Qual o tipo de conta você gostaria de cadastrar no nosso sistema?",
                    "[1] ADMINISTRADOR",
                    "[2] PROFESSOR",
                    "[3] ALUNO",
                    "Digite o NÚMERO correspondente a sua opção:"]
    option <- getLine
    let chosenOption = head option
    chooseOption chosenOption

userRegister :: IO (String, String, String, String, String, String)
userRegister = do
    screenCleaner
    userType <- selectAccountType
    screenCleaner

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

    return (userType, userName, userUniversity, userEnrollment, userEmail, userPassword)

selectAction :: IO Char 
selectAction = do
    screenCleaner
    mapM_ putStrLn ["Qual tipo de operação você gostaria de realizar no momento?",
                    "[1] CADASTRAR USUÁRIO",
                    "[2] REMOVER USUÁRIO DO SISTEMA",
                    "[3] ATUALIZAR INFORMAÇÕES DE UM USUÁRIO",
                    "[4] VALIDAR CADASTROS",
                    "[.] sair",
                    "Digite o NÚMERO correspondente a sua opção:"]
    option <- getLine
    let chosenOption = head option
    return chosenOption
