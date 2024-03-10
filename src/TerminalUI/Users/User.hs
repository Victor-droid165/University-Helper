module TerminalUI.Users.User
    ( loginUI
    , registerUI
    , typeUserName
    , typeUniversity
    , typeEnrollment
    , typeUserEmail
    , typeUserPassword
    , invalidOption
    ) where

import Util.ScreenCleaner ( screenCleaner )
import Control.Concurrent ( threadDelay )

invalidOption :: IO ()
invalidOption = do
    mapM_ putStrLn ["Opção Inválida. Tente Novamente.",
                    "Se deseja escolher a opção '[X] - Opção', digite: X\n"]

chooseOption :: Char -> IO String
chooseOption choice
    | choice == '1' = return "teacher"
    | choice == '2' = return "student"
    | otherwise = do
        invalidOption
        selectAccountType

selectAccountType :: IO String
selectAccountType = do
    mapM_ putStrLn ["Qual o tipo de conta você gostaria de cadastrar no nosso sistema?",
                    "[1] PROFESSOR",
                    "[2] ALUNO",
                    "Digite o NÚMERO correspondente a sua opção:"]
    option <- getLine
    let chosenOption = head option
    chooseOption chosenOption

typeUserName :: IO String
typeUserName = do
    mapM_ putStrLn ["Agora precisamos saber qual o nome do usuário que você irá cadastrar",
                    "Digite o NOME da pessoa que usará o sistema:"]
    username <- getLine
    return username

typeUniversity :: IO String
typeUniversity = do
    mapM_ putStrLn ["A qual universidade o usuário faz parte?",
                    "Digite o NOME da universidade que constará no sistema:"]
    university <- getLine
    return university

typeEnrollment :: IO String
typeEnrollment = do
    mapM_ putStrLn ["Agora precisamos saber qual a matrícula do usuário",
                    "Digite o numero de MATRÍCULA da pessoa que usará o sistema:"]
    enrollment <- getLine
    return enrollment

typeUserEmail :: [String] -> IO String
typeUserEmail textToShow = do
    mapM_ putStrLn textToShow
    email <- getLine
    return email

typeUserPassword :: [String] -> IO String
typeUserPassword textToShow = do
    mapM_ putStrLn textToShow
    password <- getLine
    return password

registerUI :: IO (String, String, String, String, String,String)
registerUI = do
    screenCleaner
    mapM_ putStrLn ["Bom saber que deseja utilizar nosso sistema!",
                    "Vamos agora solicitar algumas informações para que você possa ser efetivado no sistema\n"]

    userType <- selectAccountType
    screenCleaner
    userName <- typeUserName
    screenCleaner
    userUniversity <- typeUniversity
    screenCleaner
    userEnrollment <- typeEnrollment
    screenCleaner
    userEmail <- typeUserEmail ["Agora informe-nos o e-mail do usuário",
                                "Digite o E-MAIL da pessoa que utilizará o sistema:"]

    userPassword <- typeUserPassword ["Digite a SENHA que a pessoa utilizará para o login:"]
    screenCleaner
    return (userType, userName, userUniversity, userEnrollment, userEmail, userPassword)

loginUI :: IO (String, String)
loginUI = do
    screenCleaner
    putStrLn "Bem Vindo ao Login !"
    userEmail <- typeUserEmail ["E-mail:"]
    userPassword <- typeUserPassword ["Senha:"]
    return (userEmail, userPassword)