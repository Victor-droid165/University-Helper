module Models.User
    ( User(..)
    , findUser
    , userToString
    , stringToUser
    , writeUserOnFile
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

findUser :: String -> String -> IO (Maybe User)
findUser email password = do
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