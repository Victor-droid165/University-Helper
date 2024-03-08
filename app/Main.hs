module Main
    ( main
    ) where

import Util ( screenCleaner )
import Control.Concurrent ( threadDelay )
import Authentication ( login, register )

invalidOption :: IO ()
invalidOption = do
        mapM_ putStrLn ["Opção Inválida. Tente Novamente.",
                        "Se deseja escolher a opção '[X] - Opção', digite: X\n"]

frontPage :: IO ()
frontPage = do

    mapM_ putStrLn ["Bem Vindos ao UNIVERSITY HELPER !",
                    "Selecione o que deseja realizar:",
                    "[1] CADASTRAR",
                    "[2] ENTRAR\n",
                    "Digite o NÚMERO correspondente a sua opção:\n"]

    option <- getLine
    let chosenOption = head option
    screenCleaner
    choose chosenOption

choose :: Char -> IO ()
choose choice
    | choice == '1' = register
    | choice == '2' = login
    | otherwise = do
        invalidOption
        frontPage

main :: IO ()
main = do
    screenCleaner
    frontPage