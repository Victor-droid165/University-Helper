{-# LANGUAGE OverloadedStrings #-}
module Util.Validate (
    userNameValidation,
    userRegisterEmailValidation,
    userLoginEmailValidation,
    userPasswordValidation,
    userEnrollmentValidation,
    userUniversityValidation,
    FormValidation(..),
    Validation(..),
    Error(..),
    handleValidation
) where
import Data.Char (isAlpha, isSpace)
import Text.Email.Validate 
import qualified Data.ByteString.Char8 as B
import Util.ScreenCleaner (screenCleaner)

data Validation err a = Failure err | Success a
  deriving Show

data Error =
    EmptyField
  | NotString
  | NotMinLength
  | NotAValidEmail
  | NotAValidEnroll
  deriving Show

type FormValidation = Validation [Error]

-- main validate function
validateAll :: [String -> FormValidation String] -> String -> FormValidation String
validateAll validations str = foldl aggregate (Success str) validations
  where
    aggregate :: FormValidation String -> (String -> FormValidation String) -> FormValidation String
    aggregate (Failure errs) _ = Failure errs
    aggregate (Success _) validation = validation str

-- select what'll validate for each type
userNameValidation :: String -> FormValidation String
userNameValidation name = validateAll [notEmpty, notNum] name

userRegisterEmailValidation :: String -> FormValidation String
userRegisterEmailValidation email = validateAll [notEmpty, validateEmail, validateAdminEmail] email

userLoginEmailValidation :: String -> FormValidation String
userLoginEmailValidation email = validateAll [notEmpty, validateEmail] email

userPasswordValidation :: String -> FormValidation String
userPasswordValidation password = validateAll [minLength, notEmpty] password

userEnrollmentValidation :: String -> FormValidation String
userEnrollmentValidation enroll = validateAll [enrollSize, enrollValidation] enroll

userUniversityValidation :: String -> FormValidation String
userUniversityValidation university = validateAll [notEmpty, notNum] university

-- validate functions
notEmpty :: String -> FormValidation String
notEmpty "" = Failure [EmptyField]
notEmpty str = Success str

notNum :: String -> FormValidation String
notNum str
    | isChar str = Success str
    | otherwise = Failure [NotString]

isChar :: [Char] -> Bool
isChar str = all (\c -> isAlpha c || isSpace c) str

validateAdminEmail :: String -> FormValidation String
validateAdminEmail email  | email == "everton@admin.ufcg.edu.br" = Failure [NotAValidEmail]
                          | otherwise = Success email

minLength :: String -> FormValidation String
minLength password  | length password > 7 = Success password
                    | otherwise = Failure [NotMinLength]

enrollSize :: String -> FormValidation String
enrollSize enroll   | length enroll == 10 = Success enroll
                    | otherwise = Failure [NotAValidEnroll]

enrollValidation :: String -> FormValidation String
enrollValidation enroll  | head enroll == '1' && (enrollValidation2 (tail enroll))  = Success enroll
                         | otherwise = Failure [NotAValidEnroll]

enrollValidation2 :: String -> Bool
enrollValidation2 enroll  | stringToInt (take 4 enroll) > 1950 && stringToInt (take 4 enroll) < 2025 && enrollValidation3 (drop 4 enroll) = True
                          | otherwise = False

enrollValidation3 :: String -> Bool
enrollValidation3 enroll  | head enroll == '1' && enrollValidation4 (tail enroll) = True
                          | head enroll == '2' && enrollValidation4 (tail enroll) = True
                          | otherwise = False

enrollValidation4 :: String -> Bool
enrollValidation4 enroll  | length enroll == 4 = True
                          | otherwise = False
              

validateEmail :: String -> FormValidation String
validateEmail email | not  (isValid (B.pack email)) = Failure[NotAValidEmail]
                    | otherwise = Success email

stringToInt :: String -> Int
stringToInt str = read str

-- validation handlers

handleValidation :: FormValidation String -> IO String -> IO String -> IO String
handleValidation (Failure err) _ actionIfFailure = do
  screenCleaner
  putStrLn $ "Erro: " ++ show err
  actionIfFailure
handleValidation _ actionIfSuccess _ = do actionIfSuccess