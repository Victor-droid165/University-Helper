module Controllers.Users.AdministratorController
  ( validateUserAPI,
    unvalidateUserAPI,
  )
where

import Controllers.Users.UserController (getLoggedUser)
import Data.Maybe (fromJust)
import Models.DBUser (DBUser (validatorId))
import Models.User (User (userEnrollment))
import Util.Database.Functions.UsersDBFunctions (selectAllFromUsersWhereAppDB, updateInUsersWhereAppDB)
-- Not working
validateUserAPI :: String -> IO ()
validateUserAPI enrollment = do
  loggedUser <- getLoggedUser
  loggedDBUser <- selectAllFromUsersWhereAppDB [("enrollment_number", "=", userEnrollment (fromJust loggedUser))]
  updateInUsersWhereAppDB [("admin_validator_id", show $ validatorId (head loggedDBUser))] [("enrollment_number", "=", enrollment)]

unvalidateUserAPI :: String -> IO ()
unvalidateUserAPI enrollment = do
  updateInUsersWhereAppDB [("admin_validator_id", "NULL")] [("enrollment_number", "=", enrollment)]