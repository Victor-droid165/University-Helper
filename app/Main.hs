module Main
    ( main
    ) where

import Util.ScreenCleaner ( screenCleaner, quitIO )
import Controllers.Users.UserController ( userRegister, userLogin )

invalidOption :: IO ()
invalidOption = do
    mapM_ putStrLn ["Opção Inválida. Tente Novamente.",
                    "Se deseja escolher a opção '[X] - Opção', digite: X\n"]

optionInterface :: IO ()
optionInterface = do

    mapM_ putStrLn ["Bem Vindo ao UNIVERSITY HELPER !",
                    "Selecione o que deseja realizar:",
                    "[1] CADASTRAR",
                    "[2] ENTRAR",
                    "[.] sair\n",
                    "Digite o NÚMERO correspondente a sua opção:"]
    option <- getLine
    let chosenOption = head option
    screenCleaner
    chooseOption chosenOption

chooseOption :: Char -> IO ()
chooseOption choice
    | choice == '1' = userRegister
    | choice == '2' = userLogin
    | choice == '.' = quitIO optionInterface
    | otherwise = do
        invalidOption
        optionInterface

main :: IO ()
main = do
    screenCleaner
    optionInterface