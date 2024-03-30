{-# LANGUAGE DeriveGeneric #-}

module Repositories.NoteRepository
  ( getNotesFromDB,
    removeNoteFromDBById,
    removeNoteFromDB,
    updateNoteInDB,
    createNoteInDB,
    countNotesFromDB,
    countNotesPrefixesFromDB,
    getDBNotesFromDB,
    getNotesFromDBWhere,
  )
where

import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.ToField (ToField)
import GHC.Generics (Generic)
import Models.DBCountResult (DBCountResult (DBCountResult))
import Models.DBNote (DBNote)
import Models.IntWrapper
import Models.Note (Note (..), fromDBNote)
import Repositories.UserRepository (getUserField)
import Util.Database.Functions.NotesDBFunctions (deleteFromNotesWhereAppDB, insertAllIntoNotesAppDB, selectAllFromNotesAppDB, selectAllFromNotesWhereAppDB, selectFromNotesAppDB, selectFromNotesWhereAppDB, updateAllInNotesWhereAppDB)

getNotesFromDB :: IO [IO Note]
getNotesFromDB = getNotesFromDBWhere ([] :: [(String, String, String)])

getDBNotesFromDB :: IO [DBNote]
getDBNotesFromDB = selectAllFromNotesAppDB

getNotesFromDBWhere :: (ToField b) => [(String, String, b)] -> IO [IO Note]
getNotesFromDBWhere conditions = map fromDBNote <$> selectAllFromNotesWhereAppDB conditions

countNotesFromDB :: IO Int
countNotesFromDB = do
  result <- selectFromNotesAppDB ["COUNT(*)"] :: IO [DBCountResult]
  evaluateResult (head result)
  where
    evaluateResult (DBCountResult count) = return count

countNotesPrefixesFromDB :: String -> IO Int
countNotesPrefixesFromDB notePrefix = do
  result <- selectFromNotesWhereAppDB ["COUNT(*)"] [("id", "LIKE", notePrefix ++ "%")] :: IO [DBCountResult]
  evaluateResult (head result)
  where
    evaluateResult (DBCountResult count) = return count

createNoteInDB :: Note -> IO ()
createNoteInDB note = do
  userId <- getUserField (creator note) "id" :: IO IntWrapper
  let newNoteValues = [noteId note, noteType note, visibility note, show $ title note, show $ subject note, content note, show $ extractInt userId]
  insertAllIntoNotesAppDB newNoteValues

updateNoteInDB :: Note -> IO ()
updateNoteInDB note = do
  let newValues = [noteId note, noteType note, visibility note, show $ title note, show $ subject note, content note]
  updateAllInNotesWhereAppDB newValues [("id", "=", noteId note)]

removeNoteFromDB :: Note -> IO ()
removeNoteFromDB = removeNoteFromDBById . noteId

removeNoteFromDBById :: String -> IO ()
removeNoteFromDBById noteId' = deleteFromNotesWhereAppDB [("id", "=", noteId')]