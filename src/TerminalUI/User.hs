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
                    "Digite a SENHA que o usuário utilizará para o login:\n"]
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
    putStrLn userType

    userName <- typeUserName
    putStrLn userName

    userEmail <- typeUserEmail
    putStrLn userEmail

    userPassword <- typeUserPassword
    putStrLn userPassword

    main userType userName userEmail userPassword



userLogin :: IO ()
userLogin = do
    screenCleaner
    putStrLn "Bem Vindo ao Login !"