module Util.Validate (
    userNameValidation,
    Validation(..),
    Error(..)
) where
import Data.Char (isAlpha)

data Validation err a = Failure err | Success a
  deriving Show

data Error = 
    EmptyField
  | NotString
  | NotMinLength
  deriving Show

type FormValidation = Validation [Error]

userNameValidation :: String -> FormValidation String
userNameValidation name = notEmpty name

notEmpty :: String -> FormValidation String
notEmpty "" = Failure [EmptyField]
notEmpty str = Success str

notNum :: String -> FormValidation String
notNum str
    | isChar str = Success str
    | otherwise = Failure [NotString]

isChar :: [Char] -> Bool
isChar str = all isAlpha str



