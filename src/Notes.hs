module Notes where

import Types

import Data.Time.LocalTime
import Data.Time.Clock
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.HashMap.Strict as HashMap

data Note = Note
  { noteType :: NoteType
  , noteId :: String
  , visibility :: Visibility
  , createdAt :: LocalTime
  , lastEditedAt :: LocalTime
  , title :: Maybe String
  , subject :: Maybe String
  } deriving (Show)

-- Função para criar uma nova anotação
createNote :: NoteType -> Visibility -> Maybe String -> Maybe String -> IO Note
createNote noteType' visibility' title' subject' = do
    timeZone        <- getCurrentTimeZone
    currentUTCTime  <- getCurrentTime
    noteId'         <- generateId noteType'
    let currentLocalTime = utcToLocalTime timeZone currentUTCTime
    return $ Note noteType' noteId' visibility' currentLocalTime currentLocalTime title' subject'

type NoteCounts = HashMap.HashMap String Int
nextNotesIds :: IORef NoteCounts
nextNotesIds = unsafePerformIO (newIORef HashMap.empty)

incrementNoteCount :: NoteType -> IO ()
incrementNoteCount noteType' = atomicModifyIORef' nextNotesIds update
  where
    update counts =
      let newCounts = HashMap.insertWith (+) (show noteType') 1 counts
      in (newCounts, ())

getNoteCount :: NoteType -> IO Int
getNoteCount noteType' = do
  counts <- readIORef nextNotesIds
  return (HashMap.lookupDefault 0 (show noteType') counts)

generateId :: NoteType -> IO String
generateId noteType' = do 
    num <- getNextNoteId noteType'
    return $ prefix noteType' ++ idSeparator ++ show num
    where 
        idSeparator = "-"
        getNextNoteId noteType' = do
            count <- getNoteCount noteType'
            incrementNoteCount noteType'
            return (count + 1)