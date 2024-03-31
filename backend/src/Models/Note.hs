{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Models.Note
  ( Note (..),
    fromDBNote,
    prefix,
    noteToString,
    stringToNote,
    writeNoteOnFile,
  )
where

import Controllers.Users.UserController (getUserById)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Builder (stringUtf8)
import Data.Time.Clock ()
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), RowParser, field)
import Database.PostgreSQL.Simple.ToField (Action (Plain), ToField (toField))
import Database.PostgreSQL.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import Lib (stringToData, writeDataOnFile)
import Models.DB.DBNote (DBNote (..))
import Models.User (User (..))

data NoteType = Reminder | StickyNote | PlainText | Warning deriving (Show, Read, Eq)

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

instance FromRow Note where
  fromRow :: RowParser Note
  fromRow = Note <$> field <*> field <*> field <*> field <*> field <*> field <*> fromRow

instance ToRow Note where
  toRow :: Note -> [Action]
  toRow (Note id' typ vis title' subj cont creator') =
    [ toField id',
      toField typ,
      toField vis,
      maybeToField title',
      maybeToField subj,
      toField cont
    ]
      ++ toRow creator'
    where
      maybeToField :: Maybe String -> Action
      maybeToField Nothing = Plain (stringUtf8 "")
      maybeToField (Just val) = toField val

instance ToJSON Note

instance FromJSON Note

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
