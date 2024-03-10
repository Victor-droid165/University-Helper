module Types ( 
    NoteType(..),
    Visibility(..)
    ) where

import Data.Time.LocalTime

data NoteType = Reminder | StickyNote | PlainText | Warning deriving (Show, Eq)

data Visibility = Private | Public deriving (Show, Eq)