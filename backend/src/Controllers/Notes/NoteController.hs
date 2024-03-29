module Controllers.Notes.NoteController
  ( getNextNoteId,
  )
where

import Repositories.NoteRepository (countNotesPrefixesFromDB)

getNextNoteId :: String -> IO String
getNextNoteId notePrefix = do
  maxCurId <- countNotesPrefixesFromDB notePrefix
  return (notePrefix ++ "-" ++ show (maxCurId + 1))