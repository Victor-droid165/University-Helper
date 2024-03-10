module Types ( 
    NoteType(..),
    Visibility(..),
    prefix
    ) where

import Data.Time.LocalTime

data NoteType = Reminder | StickyNote | PlainText | Warning deriving (Show, Eq)
data Visibility = Private | Public deriving (Show, Eq)

-- Função para retornar o prefixo de um dado tipo de anotação
prefix :: NoteType -> String
prefix Reminder     = "REM"
prefix StickyNote   = "SNS"
prefix PlainText    = "PLT"
prefix Warning      = "WAR"