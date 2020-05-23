{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Database (
    CrosswordData (..)
    , getConnection
    , closeConnection
    , createTable
    , createRecord
    , readRecord
    , updateRecord
    , testingonlyDropTable) where

import Data.Maybe (listToMaybe)

import Database.SQLite.Simple (Connection, execute, execute_, query, open, close, Query, Only(..))
import Database.SQLite.Simple.FromRow (FromRow, fromRow, field)
import Database.SQLite.Simple.QQ (sql)

data CrosswordData = CrosswordData {
    name :: String,
    author :: String,
    crossword :: String,
    symmetry :: String,
    customWords :: String} deriving (Show, Eq)

instance FromRow CrosswordData where
  fromRow = CrosswordData <$> field <*> field <*> field <*> field <*> field

createTableQuery :: Query
createTableQuery = [sql|
    CREATE TABLE IF NOT EXISTS crosswords
    (name TEXT PRIMARY KEY,
    author TEXT,
    crossword TEXT,
    symmetry TEXT,
    customWords TEXT
    )
|]

insertQuery :: Query
insertQuery = [sql|
    INSERT INTO crosswords (name, author, crossword, symmetry, customWords)
    VALUES (?, ?, ?, ?, ?)
|]

updateQuery :: Query
updateQuery = [sql|
    UPDATE crosswords SET author = ?, crossword = ?, symmetry = ?, customWords = ?
    WHERE name = ?
|]

findQuery :: Query
findQuery = [sql|
    SELECT * FROM crosswords
    WHERE name = ?
|]

getConnection :: String -> IO Connection
getConnection = open

closeConnection :: Connection -> IO ()
closeConnection = close

createTable :: Connection -> IO ()
createTable conn = execute_ conn createTableQuery

readRecord :: Connection -> String -> IO (Maybe CrosswordData)
readRecord conn crosswordName = do
    rows <- query conn findQuery (Only (crosswordName :: String))  :: IO [CrosswordData]
    return $ listToMaybe rows

createRecord :: Connection -> CrosswordData -> IO ()
createRecord conn cd =
    execute conn insertQuery (name cd, author cd, crossword cd, symmetry cd, customWords cd)

updateRecord :: Connection -> CrosswordData -> IO ()
updateRecord conn cd =
    execute conn updateQuery (author cd, crossword cd, symmetry cd, customWords cd, name cd)

testingonlyDropTable :: Connection -> IO ()
testingonlyDropTable conn =
    execute_ conn [sql| DROP TABLE crosswords |]
