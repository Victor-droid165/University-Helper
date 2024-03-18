module TerminalUI.Users.Student
  ( displayActionSelection,
  )
where

import Models.User
import Util.ScreenCleaner (screenCleaner)

actions :: [String]
actions =
  [ "REMOVER SUA CONTA",
    "SAIR"
  ]

displayActionSelection :: IO [String]
displayActionSelection = do
  screenCleaner
  putStrLn "Qual tipo de operacao voce gostaria de realizar no momento?"
  return actions