{-# LANGUAGE OverloadedStrings #-}

module DBLib
  ( connectToDB,
    executeSqlFile,
    isDBPopulated,
    isDBCreated,
    createDB,
    createDBUser,
    createSchema,
    grantAllPrivilegesOnDBToUser,
    populateDB,
    grantAllPrivilegesOnSchemaToUser,
    isSchemaCreated,
    insertIntoTable,
    selectFromTable,
    selectFromTableWhere,
  )
where

import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.ToRow (toRow)
import Database.PostgreSQL.Simple.Types (Query (..))
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

connectToDB :: String -> Int -> String -> String -> String -> IO Connection
connectToDB host port dbName' user password = do
  let connectionString =
        "host="
          ++ host
          ++ " port="
          ++ show port
          ++ " dbname="
          ++ dbName'
          ++ " user="
          ++ user
          ++ " password="
          ++ password
  connectPostgreSQL $ BS.pack connectionString

executeSqlFile :: Connection -> FilePath -> IO ()
executeSqlFile conn filePath = do
  sql <- BS.readFile filePath
  mapM_ (execute_ conn . Query) (splitSqlStatements sql)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn delimiter splittable
  | null splittable = []
  | otherwise =
      let (before, after) = break (== delimiter) splittable
       in if null after
            then [before]
            else before : splitOn delimiter (tail after)

splitSqlStatements :: BS.ByteString -> [BS.ByteString]
splitSqlStatements sql = map BS.pack $ splitOn ';' (BS.unpack sql)

isDBPopulated :: Connection -> String -> [String] -> IO Bool
isDBPopulated conn schemaName tableNames = do
  [Only populated] <- query conn (Query queryText) (schemaName : tableNames)
  return populated
  where
    queryText =
      "SELECT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema = ? AND table_name = ANY (ARRAY["
        <> BS.intercalate ", " (map (const "?") tableNames)
        <> "]))"

populateDB :: Connection -> IO ()
populateDB conn = do
  currentDir <- getCurrentDirectory

  putStrLn "Initializing database..."
  executeSqlFile conn ("" </> "backend" </> "database" </> "init.sql")
  putStrLn "Database all set up!"

isDBCreated :: Connection -> String -> IO Bool
isDBCreated conn dbName' = do
  result <- query conn "SELECT EXISTS (SELECT FROM pg_database WHERE datname = ?)" (Only dbName')
  evaluateResult result
  where
    evaluateResult [Only exists] = return exists
    evaluateResult _ = return False

createDB :: Connection -> String -> IO ()
createDB conn newDBName = do
  _ <- execute_ conn $ Query $ "CREATE DATABASE " <> BS.pack newDBName
  putStrLn $ "Database '" ++ newDBName ++ "' created successfully."

createDBUser :: Connection -> String -> String -> IO ()
createDBUser conn userName userPassword = do
  _ <- execute_ conn $ Query $ "CREATE USER " <> BS.pack userName <> " WITH PASSWORD '" <> BS.pack userPassword <> "'"
  putStrLn $ "User '" ++ userName ++ "' created successfully."

grantAllPrivilegesOnDBToUser :: Connection -> String -> String -> IO ()
grantAllPrivilegesOnDBToUser conn dbName' userName = do
  _ <- execute_ conn $ Query $ "GRANT ALL PRIVILEGES ON DATABASE " <> BS.pack dbName' <> " TO " <> BS.pack userName
  putStrLn $ "Granted all privileges to '" ++ userName ++ "' on '" ++ dbName' ++ "' successfully."

grantAllPrivilegesOnSchemaToUser :: Connection -> String -> String -> IO ()
grantAllPrivilegesOnSchemaToUser conn schemaName userName = do
  _ <- execute_ conn $ Query $ "GRANT USAGE ON SCHEMA " <> BS.pack schemaName <> " TO " <> BS.pack userName
  _ <- execute_ conn $ Query $ "GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA " <> BS.pack schemaName <> " TO " <> BS.pack userName
  _ <- execute_ conn $ Query $ "GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA " <> BS.pack schemaName <> " TO " <> BS.pack userName
  putStrLn $ "Granted all privileges to '" ++ userName ++ "' on '" ++ schemaName ++ "' successfully."

isSchemaCreated :: Connection -> String -> IO Bool
isSchemaCreated conn schemaName = do
  result <- query conn "SELECT EXISTS (SELECT 1 FROM information_schema.schemata WHERE schema_name = ?)" (Only schemaName)
  evaluateResult result
  where
    evaluateResult [Only exists] = return exists
    evaluateResult _ = return False

createSchema :: Connection -> String -> IO ()
createSchema conn schemaName = do
  _ <- execute_ conn $ Query $ "CREATE SCHEMA " <> BS.pack schemaName
  putStrLn $ "Schema '" ++ schemaName ++ "' created successfully."

insertIntoTable :: Connection -> String -> [String] -> [String] -> IO ()
insertIntoTable conn tableName cols vals = do
  let columns = BS.intercalate ", " (map BS.pack cols)
      placeholders = BS.intercalate ", " (replicate (length vals) "?")
      tableName' = BS.pack tableName
      queryText = "INSERT INTO " <> tableName' <> " (" <> columns <> ") VALUES (" <> placeholders <> ")"
  _ <- execute conn (Query queryText) (map BS.pack vals)
  putStrLn "Data inserted successfully."

selectFromTable :: (FromRow a) => Connection -> String -> [String] -> IO [a]
selectFromTable conn tableName columns = selectFromTableWhere conn tableName columns ([] :: [(String, String, BS.ByteString)])

selectFromTableWhere :: (FromRow a, ToField b) => Connection -> String -> [String] -> [(String, String, b)] -> IO [a]
selectFromTableWhere conn tableName columns conditions = do
  let tableNameBS = BS.pack tableName
      columnsBS = BS.intercalate ", " $ map BS.pack columns
      conditionsBS =
        if null conditions
          then BS.empty
          else
            " WHERE "
              <> BS.intercalate
                " AND "
                [conditionBS' | (col, op, _) <- conditions, let conditionBS' = BS.pack col <> " " <> BS.pack op <> " ?"]
      values = map (\(_, _, val) -> val) conditions
      queryText = "SELECT " <> columnsBS <> " FROM " <> tableNameBS <> conditionsBS
  query conn (Query queryText) values
