module TerminalUI.Users.Teacher
    ( teacherOptions
    ) where

import Models.User

teacherOptions :: User -> IO ()
teacherOptions user = do
    putStrLn "Welcome to the Teacher session!"