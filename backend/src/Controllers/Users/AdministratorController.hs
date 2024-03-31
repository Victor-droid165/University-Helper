module Controllers.Users.AdministratorController
  ( validateUserAPI,
    unvalidateUserAPI,
    getIds,
  )
where

import Models.AdminValidate (AdminV)
import Util.Database.Functions.ValidationDBFunctions (deleteFromValidationsWhereAppDB, insertAllIntoValidationsAppDB, selectAllFromValidationsAppDB)

validateUserAPI :: Int -> IO ()
validateUserAPI uid = insertAllIntoValidationsAppDB [1, uid]

unvalidateUserAPI :: String -> IO ()
unvalidateUserAPI enrollment = do
  deleteFromValidationsWhereAppDB [("enrollment_number", "=", enrollment)]

getIds :: IO [AdminV]
getIds = selectAllFromValidationsAppDB