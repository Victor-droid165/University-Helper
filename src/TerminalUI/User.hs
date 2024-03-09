module TerminalUI.User
    (   userLogin,
        userRegister
    ) where

import Util.ScreenCleaner ( screenCleaner )
import Control.Concurrent ( threadDelay )
import Controllers.UserController ( main )

invalidOption :: IO ()
invalidOption = do
    mapM_ putStrLn ["Opção Inválida. Tente Novamente.",
                    "Se deseja escolher a opção '[X] - Opção', digite: X\n"]

chooseOption :: Char -> IO String
chooseOption choice
    | choice == '1' = return "professor"
    | choice == '2' = return "aluno"
    | otherwise = do
        invalidOption
        selectAccountType

selectAccountType :: IO String
selectAccountType = do
    screenCleaner
    mapM_ putStrLn ["Qual o tipo de conta você gostaria de cadastrar no nosso sistema?",
                    "[1] PROFESSOR",
                    "[2] ALUNO",
                    "Digite o NÚMERO correspondente a sua opção:"]
    option <- getLine
    let chosenOption = head option
    screenCleaner
    chooseOption chosenOption

typeUserName :: IO String
typeUserName = do
    screenCleaner
    mapM_ putStrLn ["Agora precisamos saber qual o nome do usuário que você irá cadastrar",
                    "Digite o NOME da pessoa que usará o sistema:"]
    username <- getLine
    screenCleaner
    return username

typeUniversity :: IO String
typeUniversity = do
    screenCleaner
    mapM_ putStrLn ["A qual universidade o usuário faz parte?",
                    "Digite o NOME da universidade que constará no sistema:"]
    university <- getLine
    screenCleaner
    return university

typeEnrollment :: IO String
typeEnrollment = do
    screenCleaner
    mapM_ putStrLn ["Agora precisamos saber qual a matrícula do usuário",
                    "Digite o numero de MATRÍCULA da pessoa que usará o sistema:"]
    enrollment <- getLine
    screenCleaner
    return enrollment

typeUserEmail :: IO String
typeUserEmail = do
    screenCleaner
    mapM_ putStrLn ["Agora informe-nos o e-mail do usuário",
                    "Digite o E-MAIL da pessoa que usará o sistema:"]
    email <- getLine
    screenCleaner
    return email

typeUserPassword :: IO String
typeUserPassword = do
    screenCleaner
    mapM_ putStrLn ["Maravilha, finalmente chegamos ao último passo do cadastro!",
                    "Precisamos que você digite uma senha para esse usuário(cuidado para não esquecer)",
                    "Digite a SENHA que o usuário utilizará para o login:"]
    password <- getLine
    screenCleaner
    return password

userRegister :: IO ()
userRegister = do
    screenCleaner
    mapM_ putStrLn ["Bom saber que deseja se cadastrar no nosso sistema!",
                    "Vamos agora solicitar algumas informações para que você possa ser efetivado no sistema\n"]
    threadDelay 2000000

    userType <- selectAccountType
    userName <- typeUserName
    userUniversity <- typeUniversity
    userEnrollment <- typeEnrollment
    userEmail <- typeUserEmail
    userPassword <- typeUserPassword

    main userType userName userUniversity userEnrollment userEmail userPassword

userLogin :: IO ()
userLogin = do
    screenCleaner
    putStrLn "Bem Vindo ao Login !"