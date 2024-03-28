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
validateUserAPI :: String -> IO ()
validateUserAPI enrollment = do
  loggedUser <- getLoggedUser
  loggedDBUser <- selectAllFromUsersWhereAppDB [("enrollment_number", "=", userEnrollment (fromJust loggedUser))]
  toValidateDBUser <- selectAllFromUsersWhereAppDB [("enrollment_number", "=", enrollment)]
  insertAllIntoValidationsAppDB [(dbUserId . head) loggedDBUser, (dbUserId . head) toValidateDBUser]

unvalidateUserAPI :: String -> IO ()
unvalidateUserAPI enrollment = do
  deleteFromValidationsWhereAppDB [("enrollment_number", "=", enrollment)]

getIds :: IO [AdminV]
getIds = selectAllFromValidationsAppDB 