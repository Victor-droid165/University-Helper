{-# LANGUAGE OverloadedStrings #-}

module Util.Database.DBFunctions
  ( populateAppDBIfNotPopulated,
    isDBCreated,
    initDB,
    insertIntoAppDB,
    selectFromTableAppDB,
    selectFromTableWhereAppDB,
    selectAllFromTableAppDB,
    selectAllFromTableWhereAppDB,
  )
where

import Control.Exception
import DBLib
  ( connectToDB,
    createDB,
    createDBUser,
    createSchema,
    grantAllPrivilegesOnDBToUser,
    grantAllPrivilegesOnSchemaToUser,
    insertIntoTable,
    isDBCreated,
    isDBPopulated,
    isSchemaCreated,
    populateDB,
    selectFromTable,
    selectFromTableWhere,
  )
import qualified Data.ByteString.Char8 as BS
import Data.Yaml (FromJSON (..), decodeFileThrow)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.Types (Query (..))
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Util.Database.DBConfig

loadYamlFile :: (FromJSON a) => FilePath -> IO (Maybe a)
loadYamlFile filePath = do
  decodeFileThrow filePath `catch` handleException
  where
    handleException :: SomeException -> IO (Maybe a)
    handleException exception = do
      putStrLn $ "Error extracting data from YAML file: " ++ show exception
      return Nothing

loadDBConfig :: IO DBConfig
loadDBConfig = do
  currentDir <- getCurrentDirectory
  maybeDbConfig <- loadYamlFile ("" </> "backend" </> "database" </> "config.yaml")
  case maybeDbConfig of
    Just dbConfig' -> return dbConfig'
    Nothing -> error "Failed to load database configuration"

dbConfig :: DBConfig
dbConfig = unsafePerformIO loadDBConfig

connectToAppDB :: IO Connection
connectToAppDB = connectToDB "localhost" 5432 "plp_db" "admin" "senhaSegura"

connectToSuperUserDB :: IO Connection
connectToSuperUserDB = connectToDB (dbHost dbConfig) (dbPort dbConfig) (dbName dbConfig) (dbUser dbConfig) (dbPassword dbConfig)

createAppDB :: IO ()
createAppDB = do
  conn <- connectToSuperUserDB
  createDB conn "plp_db"
  close conn

createAppDBIfNotCreated :: IO ()
createAppDBIfNotCreated = do
  appDBExists <- isAppDBCreated
  if appDBExists
    then putStrLn "Database 'plp_db' already exists. It won't be created again."
    else do
      conn <- connectToSuperUserDB
      createDB conn "plp_db"
      close conn

populateAppDB :: IO ()
populateAppDB = do
  conn <- connectToAppDB
  populateDB conn
  close conn

populateAppDBIfNotPopulated :: IO ()
populateAppDBIfNotPopulated = do
  conn <- connectToAppDB
  populated <- isDBPopulated conn "uh_schema" ["users", "notes", "notebooks"]
  close conn
  if populated
    then putStrLn "The database had already been populated."
    else do
      putStrLn "The database is yet to be populated. Creating tables..."
      populateAppDB

createUserDB :: String -> String -> IO ()
createUserDB userName userPassword = do
  conn <- connectToSuperUserDB
  createDBUser conn userName userPassword
  close conn

createUserAppDB :: IO ()
createUserAppDB = createUserDB "admin" "senhaSegura"

createAppSchema :: IO ()
createAppSchema = do
  conn <- connectToAppDB
  createSchema conn "uh_schema"
  close conn

createAppSchemaIfNotCreated :: IO ()
createAppSchemaIfNotCreated = do
  conn <- connectToAppDB
  schemaExists <- isSchemaCreated conn "uh_schema"
  close conn
  if schemaExists
    then putStrLn "Schema 'uh_schema' already exists. It won't be created again."
    else do
      putStrLn "Creating 'uh_schema'..."
      createAppSchema

grantAllPrivilegesToAppUser :: IO ()
grantAllPrivilegesToAppUser = do
  conn <- connectToSuperUserDB
  grantAllPrivilegesOnDBToUser conn "plp_db" "admin"
  close conn

isAppDBCreated :: IO Bool
isAppDBCreated = do
  conn <- connectToSuperUserDB
  result <- isDBCreated conn "plp_db"
  close conn
  return result

insertIntoAppDB :: String -> [String] -> [String] -> IO ()
insertIntoAppDB tableName cols vals = do
  conn <- connectToAppDB
  insertIntoTable conn ("uh_schema." ++ tableName) cols vals
  close conn

selectAllFromTableAppDB :: (FromRow a) => String -> IO [a]
selectAllFromTableAppDB tableName = selectFromTableAppDB tableName ["*"]

selectFromTableAppDB :: (FromRow a) => String -> [String] -> IO [a]
selectFromTableAppDB tableName columns = do
  conn <- connectToAppDB
  result <- selectFromTable conn ("uh_schema." ++ tableName) columns
  close conn
  return result

selectAllFromTableWhereAppDB :: (FromRow a, ToField b) => String -> [(String, String, b)] -> IO [a]
selectAllFromTableWhereAppDB tableName = selectFromTableWhereAppDB tableName ["*"]

selectFromTableWhereAppDB :: (FromRow a, ToField b) => String -> [String] -> [(String, String, b)] -> IO [a]
selectFromTableWhereAppDB tableName columns conditions = do
  conn <- connectToAppDB
  result <- selectFromTableWhere conn ("uh_schema." ++ tableName) columns conditions
  close conn
  return result

initDB :: String -> IO ()
initDB dbName' = do
  appDBExists <- isAppDBCreated
  if appDBExists
    then do
      createAppSchemaIfNotCreated
      populateAppDBIfNotPopulated
    else do
      createAppDB
      createUserAppDB
      grantAllPrivilegesToAppUser
      createAppSchemaIfNotCreated
      populateAppDB

-- TODO : SCHEMA