module Lib
  ( getInput,
    getValidInput,
    getParsedInput,
    getParsedValidInput,
    handleValidation,
    handleMaybe,
    stringToData,
    writeDataOnFile,
    selectOption,
  )
where

-- Remember to import here
import System.IO
import Text.Read (readMaybe)

getInput :: Maybe String -> IO String
getInput maybePrompt = do
  handleMaybe maybePrompt (return ()) putStr
  hFlush stdout
  getLine

getParsedInput :: (Read a) => Maybe String -> IO a
getParsedInput maybePrompt = do
  value <- getInput maybePrompt
  return $ read value

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
  clearScreen
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

selectOption :: [(String, IO ())] -> IO ()
selectOption options = do
  let optionsPrompts = map fst options
  displayOptions optionsPrompts
  choice <- getUserChoice
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

handleChoice :: String -> [(String, IO ())] -> IO ()
handleChoice "." options = snd (options !! ((length options) - 1))
handleChoice choice options
  | Just index <- readMaybe choice :: Maybe Int,
    isValidIndex index 1 (length options) =
      snd (options !! (index - 1))
  | otherwise = retryChoice options

isValidIndex :: Int -> Int -> Int -> Bool
isValidIndex index minIndex maxIndex = index >= minIndex && index <= maxIndex

retryChoice :: [(String, IO ())] -> IO ()
retryChoice options = do
  putStrLn "Opcao Invalida. Tente Novamente."
  putStrLn "Se deseja escolher a opcao '[X] - Opcao', digite: X"
  selectOption options