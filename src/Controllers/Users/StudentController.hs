module Controllers.Users.StudentController
    ( studentOptions
    ) where

import Models.User

studentOptions :: IO ()
studentOptions = do
    putStrLn "Welcome to the Student Session!"