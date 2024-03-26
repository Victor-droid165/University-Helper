{-# LANGUAGE InstanceSigs #-}
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
    selectFromTableWhere,
    updateInTableWhere,
    deleteFromTableWhere,
    AnyField (..),
  )
where

import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple
  ( Connection,
    FromRow,
    Only (Only),
    connectPostgreSQL,
    execute,
    execute_,
    query,
  )
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.Types (Query (..))
import System.FilePath ((</>))

type WhereCondition b = (String, String, b)

data AnyField = IntField Int | StringField String | DoubleField Double | NullableField (Maybe AnyField)
instance Show AnyField where
  show :: AnyField -> String
  show (IntField i) = show i
  show (DoubleField d) = show d
  show (StringField s) = s
  show (NullableField (Just nf)) = show nf
  show (NullableField _) = "Nothing"

buildWhereClauses :: [WhereCondition a] -> BS.ByteString
buildWhereClauses conditions =
  if null conditions
    then BS.empty
    else " WHERE " <> BS.intercalate " AND " [BS.pack col <> " " <> BS.pack op <> " ?" | (col, op, _) <- conditions]

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

insertIntoTable :: (ToField b) => Connection -> String -> [String] -> [b] -> IO ()
insertIntoTable conn tableName cols vals = do
  let columns = map BS.pack cols
      colsBS = if null cols then BS.empty else " (" <> BS.intercalate ", " columns <> ")"
      placeholders = BS.intercalate ", " (replicate (length vals) "?")
      tableName' = BS.pack tableName
      queryText = "INSERT INTO " <> tableName' <> colsBS <> " VALUES (" <> placeholders <> ")"
  print queryText
  _ <- execute conn (Query queryText) vals
  putStrLn "Data inserted successfully."

selectFromTableWhere :: (FromRow a, ToField b) => Connection -> String -> [String] -> [WhereCondition b] -> IO [a]
selectFromTableWhere conn tableName columns conditions = do
  let tableNameBS = BS.pack tableName
      columnsBS = BS.intercalate ", " $ map BS.pack columns
      conditionsBS = buildWhereClauses conditions
      values = map (\(_, _, val) -> val) conditions
      queryText = "SELECT " <> columnsBS <> " FROM " <> tableNameBS <> conditionsBS
  query conn (Query queryText) values

updateInTableWhere :: Connection -> String -> [(String, String)] -> [WhereCondition String] -> IO ()
updateInTableWhere conn tableName updateValues conditions = do
  let tableNameBS = BS.pack tableName
      updateText =
        if null updateValues
          then BS.empty
          else " SET " <> BS.intercalate ", " [BS.pack col <> " = ?" | (col, _) <- updateValues]
      conditionsBS = buildWhereClauses conditions
      values = map (\(_, _, val) -> val) conditions
      queryText = "UPDATE " <> tableNameBS <> updateText <> conditionsBS
  _ <- execute conn (Query queryText) (map snd updateValues ++ values)
  putStrLn "Data updated successfully."

deleteFromTableWhere :: (ToField b) => Connection -> String -> [WhereCondition b] -> IO ()
deleteFromTableWhere conn tableName conditions = do
  let tableNameBS = BS.pack tableName
      conditionsBS = buildWhereClauses conditions
      values = map (\(_, _, val) -> val) conditions
      queryText = "DELETE FROM " <> tableNameBS <> conditionsBS
  _ <- execute conn (Query queryText) values
  putStrLn "Data deleted successfully."