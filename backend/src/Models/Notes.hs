module Models.Notes () where

import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.Time.Clock
import Data.Time.LocalTime
import System.IO.Unsafe (unsafePerformIO)

data NoteType = Reminder | StickyNote | PlainText | Warning deriving (Show, Eq)

data Visibility = Private | Public deriving (Show, Eq)

-- Função para retornar o prefixo de um dado tipo de anotação
prefix :: NoteType -> String
prefix Reminder = "REM"
prefix StickyNote = "SNS"
prefix PlainText = "PLT"
prefix Warning = "WAR"

data Note = Note
  { noteType :: NoteType,
    noteId :: String,
    content :: String,
    visibility :: Visibility,
    createdAt :: LocalTime,
    lastEditedAt :: LocalTime,
    title :: Maybe String,
    subject :: Maybe String
  }
  deriving (Show)

-- Função para criar uma nova anotação
createNote :: NoteType -> String -> Visibility -> Maybe String -> Maybe String -> IO Note
createNote noteType' content' visibility' title' subject' = do
  timeZone <- getCurrentTimeZone
  currentUTCTime <- getCurrentTime
  noteId' <- generateId noteType'
  let currentLocalTime = utcToLocalTime timeZone currentUTCTime
  return $ Note noteType' noteId' content' visibility' currentLocalTime currentLocalTime title' subject'

-- Definição de um tipo Map de string para inteiro
type NoteCounts = HashMap.HashMap String Int

-- Responsável por armazenar as contagens de cada tipo de anotação usando o HashMap
nextNotesIds :: IORef NoteCounts
nextNotesIds = unsafePerformIO (newIORef HashMap.empty)

-- Função para incrementar a contagem de um dado tipo de anotação
incrementNoteCount :: NoteType -> IO ()
incrementNoteCount noteType' = atomicModifyIORef' nextNotesIds update
  where
    update counts =
      let newCounts = HashMap.insertWith (+) (show noteType') 1 counts
       in (newCounts, ())

-- Função para recuperar a contagem de um dado tipo de anotação
getNoteCount :: NoteType -> IO Int
getNoteCount noteType' = do
  counts <- readIORef nextNotesIds
  return (HashMap.lookupDefault 0 (show noteType') counts)

-- Função para gerar o Id de uma anotação
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