module Main where

import Lib
import System.Console.ANSI
import Control.Concurrent
import Data.Char (toLower)

screenCleaner :: IO ()
screenCleaner = do
    clearScreen
    threadDelay 1000000

inicio :: IO ()
inicio = do
    screenCleaner
    putStrLn "Bem Vindos ao UNIVERSITY HELPER !\nSelecione o que deseja realizar:\n"
    putStrLn "[A] CADASTRAR\n[B] ENTRAR"
    putStrLn "Digite a referência da sua opção que está entre [chaves] ou o respectivo nome:\n"    
    esc <- getLine
    let escolha = map toLower esc
    choose escolha

choose :: String -> IO ()
choose esc = case esc of
    "cadastrar" -> cadastro
    "a" -> cadastro
    "[a]" -> cadastro
    "[a] cadastrar" -> cadastro
    "entrar" -> login
    "b" -> login
    "[b]" -> login
    "[b] entrar" -> login
    _ -> do
        putStrLn "Opção Inválida. Tente Novamente."
        threadDelay(2 * 1000000)
        inicio

cadastro :: IO ()
cadastro = do
    screenCleaner
    putStrLn "Bem Vindo ao Cadastro !"

login :: IO ()
login = do
    screenCleaner
    putStrLn "Bem Vindo ao Login !"

main :: IO ()
main = do
    inicio