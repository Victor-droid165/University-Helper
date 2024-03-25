module Util.ScreenCleaner
  ( screenCleaner,
    quitIO,
    forceQuit,
    quitOpt,
    start,
  )
where

import Control.Concurrent (threadDelay)
import Data.Char (toLower)
import System.Console.ANSI
import Util.Database.DBFunctions (initDB)

screenCleaner :: IO ()
screenCleaner = do
  -- threadDelay 1000000
  threadDelay 0
  clearScreen

quitIO :: IO () -> IO ()
quitIO func = do
  mapM_
    putStrLn
    [ "Deseja deslogar e sair do programa?",
      "[SIM]",
      "[NAO]"
    ]
  input <- getLine
  let lowerInput = map toLower input
  quitOpt func lowerInput

forceQuit :: IO () -> IO ()
forceQuit func = do
  quitOpt func "sim"

quitOpt :: IO () -> String -> IO ()
quitOpt _ "sim" = do
  screenCleaner
  putStrLn "Obrigado por utilizar o University Helper! Ate a proxima!"
  writeFile "backend/data/session.txt" ""
quitOpt func _ = do
  screenCleaner
  func

start :: IO ()
start = do
  screenCleaner
  initDB "plp_db"
  writeFile "backend/data/session.txt" ""