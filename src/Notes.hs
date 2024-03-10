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
generateId noteType' num
    | noteType' == Reminder = "REM-" ++ show num
    | noteType' == StickyNote = "SNS-" ++ show num
    | noteType' == PlainText = "PLT-" ++ show num
    | noteType' == Warning = "WAR-" ++ show num
    | otherwise = error "Invalid note type"