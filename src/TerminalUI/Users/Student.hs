module TerminalUI.Users.Student
    ( selectAction
    ) where

import Models.User
import Util.ScreenCleaner ( screenCleaner )

selectAction :: IO Char 
selectAction = do
    screenCleaner
    mapM_ putStrLn ["Qual tipo de operação você gostaria de realizar no momento?",
                    "[1] REMOVER SUA CONTA",
                    "[.] sair",
                    "Digite o NÚMERO correspondente a sua opção:"]
    option <- getLine
    let chosenOption = head option
    return chosenOption