module Lib
    (
      getInput,
      getValidInput,
      getParsedInput,
      getParsedValidInput,
      handleValidation,
      handleMaybe
    ) where

-- Remember to import here

getInput :: Maybe String -> IO String
getInput maybePrompt = do
    handleMaybe maybePrompt (return ()) (\prompt -> putStr prompt)
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

handleMaybe :: Maybe a -> b -> (a -> b) -> b
handleMaybe (Just value) _ function = function value
handleMaybe _ defaultValue _ = defaultValue