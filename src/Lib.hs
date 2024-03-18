module Lib
  ( getInput,
    getValidInput,
    handleValidation,
    handleMaybe,
    stringToData,
    writeDataOnFile,
    selectOption,
    joinStringArray,
    emailInputPrompts,
    passwordInputPrompt,
    userNameInputPrompts,
    universityInputPrompts,
    userEnrollmentInputPrompts,
    printWelcomeMessages,
  )
where

-- Remember to import here

import System.IO
import Text.Read (readMaybe)
import Util.ScreenCleaner (quitIO, screenCleaner)
import Util.Validate (FormValidation)
import Util.Validate hiding (handleValidation)

getInput :: Maybe String -> IO String
getInput maybePrompt = do
  handleMaybe maybePrompt (return ()) putStr
  hFlush stdout
  getLine

getValidInput :: Maybe String -> (String -> FormValidation String) -> IO String
getValidInput maybePrompt validationFunc = do
  value <- getInput maybePrompt
  handleValidation (validationFunc value) (return value) (getValidInput maybePrompt validationFunc)

getParsedValidInput :: (Read a) => Maybe String -> (String -> FormValidation String) -> IO a
getParsedValidInput maybePrompt validationFunc = do
  value <- getValidInput maybePrompt validationFunc
  return $ read value

handleValidation :: FormValidation String -> IO String -> IO String -> IO String
handleValidation (Failure err) _ actionIfFailure = do
  screenCleaner
  putStrLn $ "Erro: " ++ show err
  actionIfFailure
handleValidation _ actionIfSuccess _ = do actionIfSuccess

stringToData :: (Read a) => String -> Maybe a
stringToData str = readMaybe str

writeDataOnFile :: (Show a) => FilePath -> a -> IO ()
writeDataOnFile filePath data' = appendFile filePath (show data' ++ "\n")

handleMaybe :: Maybe a -> b -> (a -> b) -> b
handleMaybe (Just value) _ function = function value
handleMaybe _ defaultValue _ = defaultValue

selectOption :: [(String, IO a)] -> IO a
selectOption options = do
  let optionsPrompts = map fst options
  displayOptions optionsPrompts
  choice <- getUserChoice
  screenCleaner
  handleChoice choice options

displayOptions :: [String] -> IO ()
displayOptions options =
  displayOptionsHelper numberedOptions
  where
    displayOptionsHelper :: [(Int, String)] -> IO ()

    displayOptionsHelper [] = return ()
    displayOptionsHelper ((index, option) : xs) = do
      putStrLn $ "[" ++ show index ++ "] " ++ option
      displayOptionsHelper xs

    numberedOptions = zip [1 ..] options

getUserChoice :: IO String
getUserChoice = do
  putStr "Digite o NUMERO correspondente a sua opcao: "
  hFlush stdout
  getLine

handleChoice :: String -> [(String, IO a)] -> IO a
handleChoice "." options = snd (options !! ((length options) - 1))
handleChoice choice options
  | Just index <- readMaybe choice :: Maybe Int,
    isValidIndex index 1 (length options) =
      snd (options !! (index - 1))
  | otherwise = retryChoice options

isValidIndex :: Int -> Int -> Int -> Bool
isValidIndex index minIndex maxIndex = index >= minIndex && index <= maxIndex

retryChoice :: [(String, IO a)] -> IO a
retryChoice options = do
  putStrLn "Opcao Invalida. Tente Novamente."
  putStrLn "Se deseja escolher a opcao '[X] - Opcao', digite: X"
  selectOption options

joinStringArray :: [String] -> String -> String
joinStringArray [] _ = ""
joinStringArray [x] _ = x
joinStringArray (x : xs) joiner = x ++ joiner ++ joinStringArray xs joiner

emailInputPrompts :: [String]
emailInputPrompts = ["Agora informe-nos o e-mail do usuario", "Digite o E-MAIL da pessoa que utilizara o sistema: "]

passwordInputPrompt :: String
passwordInputPrompt = "Digite a SENHA que a pessoa utilizara para o login: "

userNameInputPrompts :: [String]
userNameInputPrompts =
  [ "Agora precisamos saber qual o nome do usuario que você ira cadastrar",
    "Digite o NOME da pessoa que usara o sistema: "
  ]

universityInputPrompts :: [String]
universityInputPrompts =
  [ "A qual universidade o usuario faz parte?",
    "Digite o NOME da universidade que constara no sistema: "
  ]

printWelcomeMessages :: IO ()
printWelcomeMessages = do
  screenCleaner
  putStrLn "Bom saber que deseja utilizar nosso sistema!"
  putStrLn "Vamos agora solicitar algumas informacoes para que voce possa ser efetivado no sistema\n"

userEnrollmentInputPrompts :: [String]
userEnrollmentInputPrompts =
  [ "Agora precisamos saber qual a matricula do usuario",
    "Digite o numero de MATRICULA da pessoa que usara o sistema: "
  ]
