module Controllers.Users.AdministratorController
  ( validateUserAPI,
    unvalidateUserAPI,
  )
where

import Controllers.Users.UserController (getLoggedUser)
import Data.Maybe (fromJust)
import Models.DBUser (DBUser (dbUserId))
import qualified Models.DBUser as DBUser
import Models.User (User (userEnrollment))
import Util.Database.Functions.UsersDBFunctions (selectAllFromUsersWhereAppDB, updateInUsersWhereAppDB)
import Util.Database.Functions.ValidationDBFunctions (deleteFromValidationsWhereAppDB, insertAllIntoValidationsAppDB)

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