module Repositories.ValidationRepository
  ( createAdminValidationInDB,
    getAdminValidationsFromDB,
    getAdminValidationsFromDBWhere,
    removeAdminValidationFromDBByUserId,
  )
where

import Models.AdminValidate (AdminV)
import Util.Database.Functions.ValidationDBFunctions
  ( deleteFromValidationsWhereAppDB,
    insertAllIntoValidationsAppDB,
    selectAllFromValidationsAppDB,
    selectAllFromValidationsWhereAppDB,
  )

getAdminValidationsFromDB :: IO [AdminV]
getAdminValidationsFromDB = selectAllFromValidationsAppDB

getAdminValidationsFromDBWhere :: [(String, String, String)] -> IO [AdminV]
getAdminValidationsFromDBWhere = selectAllFromValidationsWhereAppDB

createAdminValidationInDB :: Int -> IO ()
createAdminValidationInDB userId = insertAllIntoValidationsAppDB [1, userId]

removeAdminValidationFromDBByUserId :: Int -> IO ()
removeAdminValidationFromDBByUserId userId = deleteFromValidationsWhereAppDB [("user_id", "=", userId)]