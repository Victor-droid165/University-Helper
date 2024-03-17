{-# LANGUAGE OverloadedStrings #-}

module Lib (
    populateDBIfNotPopulated
) where

import DBConfig
import Data.Yaml (FromJSON(..), decodeFileThrow)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(..))
import Control.Exception
import qualified Data.ByteString.Char8 as BS
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import System.IO.Unsafe (unsafePerformIO)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn delimiter splittable
    | null splittable = []
    | otherwise =
        let (before, after) = break (== delimiter) splittable
        in if null after
            then [before]
            else before : splitOn delimiter (tail after)

loadYamlFile :: FromJSON a => FilePath -> IO (Maybe a)
loadYamlFile filePath = do
    result <- (decodeFileThrow filePath) `catch` handleException
    return result
    where
        handleException :: SomeException -> IO (Maybe a)
        handleException exception = do
            putStrLn $ "Error extracting data from YAML file: " ++ show exception
            return Nothing

loadDBConfig :: IO DBConfig
loadDBConfig = do
    currentDir <- getCurrentDirectory
    maybeDbConfig <- loadYamlFile (currentDir </> "database" </> "config.yaml")
    case maybeDbConfig of
        Just dbConfig' -> return dbConfig'
        Nothing -> error "Failed to load database configuration"

dbConfig :: DBConfig
dbConfig = unsafePerformIO loadDBConfig

connectToDB :: IO Connection
connectToDB = do
    let connectionString = 
            "host=" ++ dbHost dbConfig ++
            " port=" ++ show (dbPort dbConfig) ++
            " dbname=" ++ dbName dbConfig ++
            " user=" ++ dbUser dbConfig ++
            " password=" ++ dbPassword dbConfig
    connectPostgreSQL $ BS.pack connectionString

executeSqlFile :: Connection -> FilePath -> IO ()
executeSqlFile conn filePath = do
    sql <- BS.readFile filePath
    mapM_ (execute_ conn . Query) (splitSqlStatements sql)

splitSqlStatements :: BS.ByteString -> [BS.ByteString]
splitSqlStatements sql = map BS.pack $ splitOn ';' (BS.unpack sql)

isDBPopulated :: IO Bool
isDBPopulated = do
    conn <- connectToDB
    [Only populated] <- query_ conn "SELECT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema = 'public' AND table_name IN ('users', 'notes', 'notebooks'))"
    close conn
    return populated

populateDB :: IO ()
populateDB = do
    conn <- connectToDB

    currentDir <- getCurrentDirectory
    
    putStrLn "Iniciando o banco de dados..."
    executeSqlFile conn (currentDir </> "database" </> "init.sql")

    close conn

populateDBIfNotPopulated :: IO ()
populateDBIfNotPopulated = do
    populated <- isDBPopulated
    if populated
        then putStrLn "O Banco de Dados ja possui as tabelas. Ele nao sera populado novamente."
        else do
            putStrLn "O Banco de Dados ainda nao foi populado. Criando tabelas..."
            populateDB