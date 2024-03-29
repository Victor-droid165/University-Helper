{-# LANGUAGE DeriveGeneric #-}

module Models.Note
  ( Note (..),
    fromDBNote,
  )
where

import Controllers.Users.UserController (getUserById)
import Data.Time.Clock ()
import GHC.Generics (Generic)
import Lib (stringToData, writeDataOnFile)
import Models.DBNote (DBNote (..))
import Models.User (User (..))

data NoteType = Reminder | StickyNote | PlainText | Warning deriving (Show, Read, Eq)

data Visibility = Private | Public deriving (Show, Read, Eq)

data Note = Note
  { noteId :: String,
    noteType :: String,
    visibility :: String,
    title :: Maybe String,
    subject :: Maybe String,
    content :: String,
    creator :: User
  }
  deriving (Show, Read, Eq, Generic)

-- Função para retornar o prefixo de um dado tipo de anotação
prefix :: NoteType -> String
prefix Reminder = "REM"
prefix StickyNote = "SNS"
prefix PlainText = "PLT"
prefix Warning = "WAR"

noteToString :: Note -> String
noteToString = show

stringToNote :: String -> Maybe Note
stringToNote = stringToData

writeNoteOnFile :: FilePath -> Note -> IO ()
writeNoteOnFile = writeDataOnFile

fromDBNote :: DBNote -> IO Note
fromDBNote dbNote = do
  user <- getUserById $ dbNoteCreatorId dbNote
  return
    Note
      { noteId = dbNoteId dbNote,
        noteType = dbNoteType dbNote,
        content = dbNoteContent dbNote,
        visibility = dbNoteVisibility dbNote,
        title = dbNoteTitle dbNote,
        subject = dbNoteSubject dbNote,
        creator = user
      }
