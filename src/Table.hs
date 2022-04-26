{-# LANGUAGE OverloadedStrings #-}

module Table where

--using sqlite-simple library
import Database.SQLite.Simple
import Consts

--there are two tables: 

-- users     USER_NAME :: string x USER_ID :: INT
-- scores    USER_ID :: INT x TOP_SCORE :: INT

-- sqlite3 scoreboard.db "CREATE TABLE users (id INTEGER PRIMARY KEY, username text);" 
-- sqlite3 scoreboard.db "CREATE TABLE scores (id INTEGER PRIMARY KEY, score integer);"

-- INSERT INTO test (str) VALUES ('test string');"
-- FUNCTION SIGNATURES
  
createID :: UserName -> IO () -- ~ createID in Table.hs
createID user_name = do
  conn <- open "scoreboard.db"
  execute conn "INSERT INTO users (username) VALUES (?)" (Only user_name)
  close conn

writeScoreToTable :: ScoreValue -> Connection -> IO ()
writeScoreToTable scoreVal conn = do
  execute conn "INSERT INTO scores (score) VALUES (?)" (Only scoreVal) 

getTop5 :: Connection -> IO ScoresList
getTop5 conn = do query_ conn "SELECT username, MAX(score) FROM scores left join \
  \users on users.id = scores.id group by username ORDER BY score DESC limit 5" :: IO ScoresList