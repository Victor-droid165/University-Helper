module Controllers.UserController    
    ( main
    ) where

data User = User
    { userId :: Int
    , userType :: String
    , userName :: String
    , userEmail :: String
    , userPassword :: String
    } deriving (Show)

userToString :: User -> String
userToString user =
    show (userId user) ++ "," ++
    userType user ++ "," ++
    userName user ++ "," ++
    userEmail user ++ "," ++
    userPassword user ++ "\n"

writeUsersToFile :: FilePath -> User -> IO ()
writeUsersToFile filePath user = writeFile filePath (userToString user)

main :: String -> String -> String -> String -> IO ()
main userType userName userEmail userPassword = do
    let newUser = User { userId = 1
                       , userType = userType
                       , userName = userName
                       , userEmail = userEmail
                       , userPassword = userPassword }

    writeUsersToFile "data/users.txt" newUser
