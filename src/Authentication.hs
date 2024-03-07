module Authentication
    (   login,
        register
    ) where

import Util ( screenCleaner )

register :: IO ()
register = do
    screenCleaner
    putStrLn "Bem Vindo ao Cadastro!"

login :: IO ()
login = do
    screenCleaner
    putStrLn "Bem Vindo ao Login !"