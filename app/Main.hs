module Main (main) where

import Data.ByteString.Char8 (pack)
import Database.PostgreSQL.Simple
import DBConfig

main :: IO ()
main = do
    -- Load database configuration from config.yaml
    maybeConfig <- loadDBConfig "config.yaml"
    case maybeConfig of
        Nothing -> putStrLn "Failed to load database configuration."
        Just dbConfig -> do
            -- Use dbConfig to establish the database connection
            putStrLn "Loaded database configuration successfully."
            putStrLn $ "Database Host: " ++ dbHost dbConfig
            putStrLn $ "Database Port: " ++ show (dbPort dbConfig)
            putStrLn $ "Database Name: " ++ dbName dbConfig
            putStrLn $ "Database User: " ++ dbUser dbConfig
            putStrLn $ "Database Password: " ++ dbPassword dbConfig

            -- Establish the database connection
            let connStr = "host=" ++ dbHost dbConfig ++ " port=" ++ show (dbPort dbConfig) ++ " dbname=" ++ dbName dbConfig ++ " user=" ++ dbUser dbConfig ++ " password=" ++ dbPassword dbConfig
            conn <- connectPostgreSQL (pack connStr)
            putStrLn "Connected to PostgreSQL database!"
            -- Perform database operations here
            -- Don't forget to close the connection when done
            close conn