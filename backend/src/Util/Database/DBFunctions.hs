{-# LANGUAGE OverloadedStrings #-}

module Util.Database.DBFunctions
  ( populateAppDBIfNotPopulated,
    initDB,
    insertIntoTableAppDB,
    insertAllIntoTableAppDB,
    selectFromTableAppDB,
    selectFromTableWhereAppDB,
    selectAllFromTableAppDB,
    selectAllFromTableWhereAppDB,
    connectToAppDB,
    updateInTableWhereAppDB,
    updateInTableAppDB,
    deleteFromTableWhereAppDB,
    deleteFromTableAppDB,
  )
where

import Control.Exception (SomeException, catch)
import DBLib
  ( connectToDB,
    createDB,
    createDBUser,
    createSchema,
    deleteFromTableWhere,
    grantAllPrivilegesOnDBToUser,
    insertIntoTable,
    isDBCreated,
    isDBPopulated,
    isSchemaCreated,
    populateDB,
    selectFromTableWhere,
    updateInTableWhere,
  )
import qualified Data.ByteString.Char8 as BS
import Data.Yaml (FromJSON (..), decodeFileThrow)
import Database.PostgreSQL.Simple (Connection, FromRow, close)
import Database.PostgreSQL.Simple.ToField (ToField)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Util.Database.DBConfig
  ( DBConfig (dbHost, dbName, dbPassword, dbPort, dbUser),
  )

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

insertAllIntoTableAppDB :: (ToField a) => String -> [a] -> IO ()
insertAllIntoTableAppDB tableName = insertIntoTableAppDB tableName ([] :: [String])

insertIntoTableAppDB :: (ToField a) => String -> [String] -> [a] -> IO ()
insertIntoTableAppDB tableName cols vals = do
  conn <- connectToAppDB
  insertIntoTable conn ("uh_schema." ++ tableName) cols vals
  close conn

selectAllFromTableAppDB :: (FromRow a) => String -> IO [a]
selectAllFromTableAppDB tableName = selectFromTableAppDB tableName ["*"]

selectFromTableAppDB :: (FromRow a) => String -> [String] -> IO [a]
selectFromTableAppDB tableName columns = selectFromTableWhereAppDB tableName columns ([] :: [(String, String, BS.ByteString)])

selectAllFromTableWhereAppDB :: (FromRow a, ToField b) => String -> [(String, String, b)] -> IO [a]
selectAllFromTableWhereAppDB tableName = selectFromTableWhereAppDB tableName ["*"]

selectFromTableWhereAppDB :: (FromRow a, ToField b) => String -> [String] -> [(String, String, b)] -> IO [a]
selectFromTableWhereAppDB tableName columns conditions = do
  conn <- connectToAppDB
  result <- selectFromTableWhere conn ("uh_schema." ++ tableName) columns conditions
  close conn
  return result

updateInTableAppDB :: (ToField b) => String -> [(String, b)] -> IO ()
updateInTableAppDB tableName updateValues = updateInTableWhereAppDB tableName updateValues ([] :: [(String, String, b)])

updateInTableWhereAppDB :: (ToField b) => String -> [(String, b)] -> [(String, String, b)] -> IO ()
updateInTableWhereAppDB tableName updateValues conditions = do
  conn <- connectToAppDB
  updateInTableWhere conn ("uh_schema." ++ tableName) updateValues conditions
  close conn

deleteFromTableAppDB :: String -> IO ()
deleteFromTableAppDB tableName = deleteFromTableWhereAppDB tableName ([] :: [(String, String, BS.ByteString)])

deleteFromTableWhereAppDB :: (ToField b) => String -> [(String, String, b)] -> IO ()
deleteFromTableWhereAppDB tableName conditions = do
  conn <- connectToAppDB
  deleteFromTableWhere conn ("uh_schema." ++ tableName) conditions
  close conn

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