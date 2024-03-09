module Controllers.UserController    
    ( userRegister
    ) where

import Data.Maybe (mapMaybe)
import Data.Foldable (find)

data User = User
    { userType :: String
    , userName :: String
    , userUniversity :: String
    , userEnrollment :: String
    , userEmail :: String
    , userPassword :: String
    } deriving (Show, Read)

findUser :: String -> IO (Maybe User)
findUser enrollment = do
    content <- readFile "data/users.txt"
    let users = mapMaybe stringToUser (lines content)
    return $ find (\user -> userEnrollment user == enrollment) users
    
userToString :: User -> String
userToString = show

stringToUser :: String -> Maybe User
stringToUser line = case reads line of
    [(user, "")] -> Just user
    _            -> Nothing

writeUsersToFile :: FilePath -> User -> IO ()
writeUsersToFile filePath user = appendFile filePath (userToString user ++ "\n")

userRegister :: String -> String -> String -> String -> String -> String -> IO()
userRegister userType userName userUniversity userEnrollment userEmail userPassword = do
    let newUser = User { userType = userType
                       , userName = userName
                       , userUniversity = userUniversity
                       , userEnrollment = userEnrollment
                       , userEmail = userEmail
                       , userPassword = userPassword }

    writeUsersToFile "data/users.txt" newUser

    --only for testing the "findUser" function
    maybeUser <- findUser userEnrollment
    print maybeUser
    case maybeUser of
        Just user -> print user
        Nothing   -> putStrLn "User not found"