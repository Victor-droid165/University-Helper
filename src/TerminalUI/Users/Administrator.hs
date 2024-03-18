module TerminalUI.Users.Administrator
  ( displayActionSelection,
    userRegister,
  )
where

import Models.User
import TerminalUI.Users.User (typeEnrollment, typeUniversity, typeUserEmail, typeUserName, typeUserPassword)
import Util.ScreenCleaner (screenCleaner)

invalidOption :: IO ()
invalidOption = do
  mapM_
    putStrLn
    [ "Opção Inválida. Tente Novamente.",
      "Se deseja escolher a opção '[X] - Opção', digite: X\n"
    ]

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
  mapM_
    putStrLn
    [ "Qual o tipo de conta você gostaria de cadastrar no nosso sistema?",
      "[1] ADMINISTRADOR",
      "[2] PROFESSOR",
      "[3] ALUNO",
      "Digite o NÚMERO correspondente a sua opção:"
    ]
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

  userEnrollment <-
    typeEnrollment
      [ "Agora precisamos saber qual a matrícula do usuário",
        "Digite o numero de MATRÍCULA da pessoa que usará o sistema:"
      ]
  screenCleaner

  userEmail <-
    typeUserEmail
      [ "Agora informe-nos o e-mail do usuário",
        "Digite o E-MAIL da pessoa que utilizará o sistema:"
      ]
      "register"

  userPassword <- typeUserPassword ["Digite a SENHA que a pessoa utilizará para o login:"]
  screenCleaner

  return (userType, userName, userUniversity, userEnrollment, userEmail, userPassword)

actions :: [String]
actions =
  [ "CADASTRAR USUARIO",
    "REMOVER USUARIO DO SISTEMA",
    "ATUALIZAR INFORMACOES DE UM USUARIO",
    "VALIDAR CADASTROS",
    "SAIR"
  ]

displayActionSelection :: IO [String]
displayActionSelection = do
  screenCleaner
  putStrLn "Qual tipo de operacao voce gostaria de realizar no momento?"
  return actions
