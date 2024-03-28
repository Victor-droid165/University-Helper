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
import System.Console.ANSI ( clearScreen )
import Util.Database.DBFunctions (initDB, isAppDBCreated)
import Util.Database.Functions.UsersDBFunctions (insertAllIntoUsersAppDB)
import Util.Database.Functions.ValidationDBFunctions (insertAllIntoValidationsAppDB)

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

registerADMIN :: IO ()
registerADMIN = do
  let newUserValues = ["everton", "everton@admin.ufcg.edu.br", "senhaSegura", "Admin", "1195010000", "UFCG"]
  insertAllIntoUsersAppDB newUserValues  
  insertAllIntoValidationsAppDB [1 :: Integer, 1 :: Integer]

start :: IO ()
start = do
  screenCleaner
  check <- isAppDBCreated
  if check
    then do
      writeFile "backend/data/session.txt" ""
    else do
      initDB "plp_db"
      registerADMIN
      writeFile "backend/data/session.txt" ""
