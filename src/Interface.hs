module Interface 
    ( frontPage
    ) where

import ScreenCleaner
import Data.Char (toLower)
import Control.Concurrent ( threadDelay )
import Authentication (login, register)

invalidOption :: IO ()
invalidOption = do
        putStrLn "Opção Inválida. Tente Novamente."
        putStrLn "Se deseja escolher a opção '[X] - Opção', digite: X"
        threadDelay(2 * 1000000)

frontPage :: IO ()
frontPage = do
    screenCleaner

    putStrLn "Bem Vindos ao UNIVERSITY HELPER !\nSelecione o que deseja realizar:"
    putStrLn "[A] CADASTRAR"
    putStrLn "[B] ENTRAR\n"
    putStrLn "Digite a LETRA correspondente a sua opção:\n"    
    
    cho <- getLine
    let choice = map toLower cho
    choose choice

choose :: String -> IO ()
choose choice = case choice of
    "a" -> register
    "b" -> login
    _ -> do
        invalidOption
        frontPage
