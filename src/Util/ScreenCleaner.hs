module Util.ScreenCleaner
    (   screenCleaner
        , quitIO
        , forceQuit
   ) where

import Control.Concurrent ( threadDelay )
import System.Console.ANSI
import Data.Char (toLower)


screenCleaner :: IO ()
screenCleaner = do
    -- threadDelay 1000000
    threadDelay 0
    clearScreen

quitIO :: IO () -> IO ()
quitIO func = do
    mapM_ putStrLn ["Deseja deslogar e sair do programa?",
                    "[SIM]",
                    "[NÃO]"]
    input <- getLine
    let lowerInput = map toLower input
    quitOpt func lowerInput

forceQuit :: IO () -> IO ()
forceQuit func = do
    quitOpt func "sim"

quitOpt :: IO () -> String -> IO ()
quitOpt _ "sim" = screenCleaner >> putStrLn "Obrigado por utilizar University Helper! Até a próxima!" >> writeFile "data/session.txt" ""
quitOpt func _  = screenCleaner >> func