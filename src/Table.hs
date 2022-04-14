module Table where

{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow


--using SQLite-Simple library
--there are two tables: 

-- USER_NAME :: string x USER_ID :: INT
-- USER_ID :: INT x TOP_SCORE :: INT

--TYPES
type UserName = String 
type UserID = Int
type ScoreValue = Int

-- FUNCTION SIGNATURES

userInput :: IO
userInput = do
--connect to SQL database and...
--run createID() with input name, for example "Oleg"

createID :: UserName -> IO
-- create ID based on one's username; function used when game starts
-- function searches through the FIRST TABLE looking for the same name 
-- if no match, then a new ID is added

writeScore :: UserID -> ScoreValue -> IO
-- connect to SQL database and write the player's score into the second talbe (only if higher than last result)
-- function is run when game over

getScoreBoard :: IO
-- run when 'S' pressed before the game start
-- run getScoreFromSql in order to get tuples and show scoreboard

getScoreFromSQL :: [(userName, SQLData)] -> [(userName, Score)] 
-- here the SQLData is SQLInteger, getting a list of tuples to draw on the scoreboard