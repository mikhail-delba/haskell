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

--TYPES
type UserName = String 
type UserID = Int 
type ScoreValue = Int

data UserField = UserField Int String deriving (Show) -- field for 'users' table

data ScoreField = ScoreField Int Int deriving (Show) -- field for 'scores' table

instance FromRow UserField where fromRow = UserField <$> field <*> field
instance FromRow ScoreField where fromRow = ScoreField <$> field <*> field

instance ToRow ScoreField where
  toRow (ScoreField id_ str) = toRow (id_, str)

-- FUNCTION SIGNATURES

createID :: UserName -> IO () -- ~ createID in Table.hs
createID userName = do
  conn <- open "scoreboard.db"
  execute conn "INSERT INTO users (username) VALUES (?)" (Only userName) -- maxID + 1 instead of ?
  --print "user added to USERS"
  close conn

writeScoreToTable :: ScoreValue -> Connection -> IO ()
writeScoreToTable scoreVal conn = do
  execute conn "INSERT INTO scores (score) VALUES (?)" (Only scoreVal)  -- NEED TO WRITE: (id, score)...

userInput :: IO( )
userInput = do
  conn <- open "scoreboard.db"
  createID "Oleg"
  close conn

getTop5 :: Connection -> IO ScoresList
getTop5 conn = do
  topList <- query_ conn "SELECT username, MAX(score) FROM scores left join users on users.id = scores.id group by username ORDER BY score DESC limit 10" :: IO ScoresList
  return topList

--DEBUG
main :: IO ()
-- better call createID here...
main = do
  u <- userInput
  putStrLn (show u)
  --w <- writeScoreToTable 105 -- EVERY TIME HAS TO BE NEW
  --putStrLn (show w)
  conn <- open "scoreboard.db"
  r <- query_ conn "SELECT * from users" :: IO [UserField]
  mapM_ print r
  r2 <- query_ conn "SELECT * from scores" :: IO [ScoreField]
  mapM_ print r2
  topList <- query_ conn "SELECT username, MAX(score) FROM scores left join users on users.id = scores.id group by username ORDER BY score DESC limit 10" :: IO [(String, Int)]
  mapM_ print topList
  print topList
  close conn