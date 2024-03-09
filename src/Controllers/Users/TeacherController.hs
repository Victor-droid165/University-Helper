module Controllers.Users.TeacherController
    ( teacherOptions
    ) where

import Models.User

teacherOptions :: IO ()
teacherOptions = do
    putStrLn "Welcome to the Teacher session!"