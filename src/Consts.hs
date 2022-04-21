module Consts where

import Graphics.Gloss
import Database.SQLite.Simple

-- CONSTANTS --

-- window size
windowWidth, windowHeight :: Int
windowWidth = 500
windowHeight = 500

paddleMaxX :: Float
paddleMaxX = (fromIntegral windowWidth/2) - paddleLength - wallWidth

-- wall
wallWidth:: Float 
wallWidth = 10

-- paddle
paddleWidth, paddleLength, paddleSpeed :: Float
paddleWidth = 10
paddleLength = 50
paddleSpeed = 10

-- ball
ballRad :: Float
ballRad = 10.0

-- сolors
fieldColor :: Color
fieldColor = dark green

ballColor :: Color
ballColor = white

paddleColor :: Color
paddleColor = light (light azure)

wallColor :: Color
wallColor = greyN 0.2

borderColor :: Color
borderColor = light green

-- Initial values --
initBallPos :: (Float, Float)
initBallPos = (0.0, 0.0)

initDir :: (Float, Float)
initDir = (1.0, 1.0)

initSpeed :: Float
initSpeed = 200.0

initPaddlePos :: (Float, Float)
initPaddlePos = (0.0, -150.0)

initialState :: Connection -> ScoresList -> GameState
initialState conn scores = GS initBallPos initDir initSpeed initPaddlePos NoMovement 0 False False False conn scores

-- DATA/TYPES --
type Pos = (Float, Float)
type Dir = (Float, Float)

data Move = MoveRight | MoveLeft | NoMovement deriving Show

type ScoresList = [(String, Int)]

-- structure to store current game's data
data GameState = GS 
  { ballPos :: Pos,
    ballDir :: Dir,
    ballSpeed :: Float,
    paddlePos :: Pos,
    paddleMove :: Move,
    score :: Int,
    scoreBoardShow :: Bool, -- when 'S' pressed && gameStarted == False, show the scoreboard
    gameStarted :: Bool, -- has the player started playing (press 'P')? if lost, then false => press 'P' again
    gameOver :: Bool, -- player lost
    connection :: Connection,
    scoresList :: ScoresList
  } 