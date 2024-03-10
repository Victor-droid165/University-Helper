module Models.User
    ( User(..)
    , authenticateUser
    , userToString
    , stringToUser
    , writeUserOnFile
    , showUser
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

authenticateUser :: String -> String -> IO (Maybe User)
authenticateUser email password = do
    content <- readFile "data/users.txt"
    let users = mapMaybe stringToUser (lines content)
    return $ find (\user -> userEmail user == email && userPassword user == password) users
    
userToString :: User -> String
userToString = show

stringToUser :: String -> Maybe User
stringToUser line = case reads line of
    [(user, "")] -> Just user
    _            -> Nothing

writeUserOnFile :: FilePath -> User -> IO ()
writeUserOnFile filePath user = appendFile filePath (userToString user ++ "\n")

userTypeToString :: User -> String
userTypeToString user   | userType user == "teacher" = "Professor"
                        | userType user == "student" = "Aluno"
                        | userType user == "administrator" = "Administrador"
                        | otherwise = "NotValidType"

showUser :: User -> IO ()
showUser user = do
    mapM_ putStr [userName user, " - ", userEnrollment user, " (", userTypeToString user, ")\n",
                    userUniversity user, "\n",
                    userEmail user, "\n"]
    