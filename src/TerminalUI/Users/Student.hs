module TerminalUI.Users.Student
    ( studentOptions
    ) where

import Models.User

studentOptions :: User -> IO ()
studentOptions user = do
    putStrLn "Welcome to the Student Session!"