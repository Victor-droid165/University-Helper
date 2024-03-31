module Controllers.Users.AdministratorController
  ( validateUserAPI,
    unvalidateUserAPI,
    getIds
  )
where

import Controllers.Users.UserController (getLoggedUser)
import Data.Maybe (fromJust)
import Models.DBUser (DBUser (dbUserId))
import Models.User (User (userEnrollment))
import Util.Database.Functions.UsersDBFunctions (selectAllFromUsersWhereAppDB)
import Util.Database.Functions.ValidationDBFunctions (deleteFromValidationsWhereAppDB, insertAllIntoValidationsAppDB, selectAllFromValidationsAppDB)
import Models.AdminValidate (AdminV)

-- Not working
validateUserAPI :: Integer -> IO ()
validateUserAPI uid = insertAllIntoValidationsAppDB [1 :: Integer, uid]


unvalidateUserAPI :: String -> IO ()
unvalidateUserAPI enrollment = do
  deleteFromValidationsWhereAppDB [("enrollment_number", "=", enrollment)]

getIds :: IO [AdminV]
getIds = selectAllFromValidationsAppDB 