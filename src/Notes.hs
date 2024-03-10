module Notes where

import Types

import Data.Time.LocalTime
import Data.Time.Clock

data Note = Note
  { noteType :: NoteType
  , noteId :: String
  , visibility :: Visibility
  , createdAt :: LocalTime
  , lastEditedAt :: LocalTime
  , title :: Maybe String
  , subject :: Maybe String
  } deriving (Show)

createNote :: NoteType -> String -> Visibility -> Maybe String -> Maybe String -> IO Note
createNote noteType' noteId' visibility' title' subject' = do
    timeZone <- getCurrentTimeZone
    currentUTCTime <- getCurrentTime 
    let currentLocalTime = utcToLocalTime timeZone currentUTCTime
    return $ Note noteType' noteId' visibility' currentLocalTime currentLocalTime title' subject'

generateId :: NoteType -> Int -> String
generateId noteType' num = prefix noteType' ++ idSeparator ++ show num
    where idSeparator = "-"