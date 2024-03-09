module Controllers.UserController    
    ( main
    ) where

data User = User
    { userType :: String
    , userName :: String
    , userEmail :: String
    , userUniversity :: String
    , userEnrollment :: String
    , userPassword :: String
    } deriving (Show)

findUser :: String -> Maybe User
findUser enrollment = do 
    content <- readFile "data/users.txt"
    let linesOfContent = lines content
    mapM_ (print . stringToUser) linesOfContent
    
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

main :: String -> String -> String -> String -> String -> String -> IO ()
main userType userName userUniversity userEnrollment userEmail userPassword = do
    let newUser = User { userType = userType
                       , userName = userName
                       , userUniversity = userUniversity
                       , userEnrollment = userEnrollment
                       , userEmail = userEmail
                       , userPassword = userPassword }

    writeUsersToFile "data/users.txt" newUser

    let retorno = stringToUser (userToString newUser)
    case retorno of
        Just user -> print user
        Nothing   -> putStrLn "Erro ao processar usuário"

    findUser
