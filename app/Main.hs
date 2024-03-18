module Main
  ( main,
  )
where

import Controllers.Users.UserController (userLogin, userRegister)
import Lib (selectOption)
import Util.ScreenCleaner (quitIO, screenCleaner)

optionInterface :: IO ()
optionInterface = do
  putStrLn "Bem Vindo ao UNIVERSITY HELPER!"
  selectOption $ zip ["CADASTRAR", "ENTRAR", "SAIR"] [userRegister, userLogin, quitIO optionInterface]

main :: IO ()
main = do
  screenCleaner
  optionInterface