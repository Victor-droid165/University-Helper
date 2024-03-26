{-# LANGUAGE FlexibleContexts #-}
module Main
  ( main,
  )
where

import Controllers.Users.UserController (userLogin, userRegister)
import Lib (printWelcomeMessages, selectOption)
import Util.ScreenCleaner (quitIO, screenCleaner)
import Util.Server (serveOn)

optionInterface :: IO ()
optionInterface = do
  putStrLn "Bem Vindo ao UNIVERSITY HELPER!"
  selectOption $ zip ["CADASTRAR", "ENTRAR", "SAIR"] [printWelcomeMessages >> userRegister, userLogin, quitIO optionInterface]

main :: IO ()
main = do
  screenCleaner
  serveOn
  --optionInterface
