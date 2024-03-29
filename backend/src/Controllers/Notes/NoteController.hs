module Controllers.Notes.NoteController
  ( getNextNoteId,
    registerNote,
  )
where

import Models.Note
import Repositories.NoteRepository (countNotesPrefixesFromDB, createNoteInDB)

getNextNoteId :: String -> IO String
getNextNoteId notePrefix = do
  maxCurId <- countNotesPrefixesFromDB notePrefix
  return (notePrefix ++ "-" ++ show (maxCurId + 1))

registerNote :: Note -> IO ()
registerNote = createNoteInDB
