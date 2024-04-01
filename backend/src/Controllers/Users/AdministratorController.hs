module Controllers.Users.AdministratorController
  ( validateUserAPI,
    unvalidateUserAPI,
    getIds,
    warnUser,
  )
where

import Models.AdminValidate (AdminV (userId))
import Models.DB.DBWarningNotification (DBWarningNotification)
import Repositories.NoteRepository (createWarningNotificationInDB)
import Repositories.ValidationRepository (createAdminValidationInDB, getAdminValidationsFromDB, removeAdminValidationFromDBByUserId)
import Util.Database.Functions.ValidationDBFunctions (deleteFromValidationsWhereAppDB, insertAllIntoValidationsAppDB, selectAllFromValidationsAppDB)

validateUserAPI :: Int -> IO ()
validateUserAPI = createAdminValidationInDB

unvalidateUserAPI :: Int -> IO ()
unvalidateUserAPI = removeAdminValidationFromDBByUserId

getIds :: IO [AdminV]
getIds = getAdminValidationsFromDB

warnUser :: DBWarningNotification -> IO ()
warnUser = createWarningNotificationInDB