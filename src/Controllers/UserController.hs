module Controllers.UserController    
    ( main
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
    } deriving (Show)

findUser :: String -> IO (Maybe User)
findUser enrollment = do
    content <- readFile "data/users.txt"
    let users = mapMaybe stringToUser (lines content)
    return $ find (\user -> userEnrollment user == enrollment) users
    
userToString :: User -> String
userToString user =
    userType user ++ " " ++
    userName user ++ " " ++
    userUniversity user ++ " " ++
    userEnrollment user ++ " " ++
    userEmail user ++ " " ++
    userPassword user ++ "\n"

stringToUser :: String -> Maybe User
stringToUser line
    | True = Just $ User userType userName userUniversity userEnrollment userEmail userPassword
    where
        [userType, userName, userUniversity, userEnrollment, userEmail, userPassword] = words line

writeUsersToFile :: FilePath -> User -> IO ()
writeUsersToFile filePath user = appendFile filePath (userToString user)

main :: String -> String -> String -> String -> String -> String -> IO()
main userType userName userUniversity userEnrollment userEmail userPassword = do
    let newUser = User { userType = userType
                       , userName = userName
                       , userUniversity = userUniversity
                       , userEnrollment = userEnrollment
                       , userEmail = userEmail
                       , userPassword = userPassword }

    writeUsersToFile "data/users.txt" newUser

    --only for test the "findUser" function
    maybeUser <- findUser userEnrollment
    print maybeUser
    case maybeUser of
        Just user -> print user
        Nothing   -> putStrLn "User not found"