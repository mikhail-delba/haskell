module Consts where

import Graphics.Gloss

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

initialState :: GameState
initialState = GS initBallPos initDir initSpeed initPaddlePos NoMovement False 0

-- DATA/TYPES --
type Pos = (Float, Float)
type Dir = (Float, Float)

data Move = MoveRight | MoveLeft | NoMovement deriving Show

data ScoreBoard = Scores {} -- a table of player's scores, storing top 5 up to date (IND. PART)

-- structure to store current game's data
data GameState = GS 
  { ballPos :: Pos,
    ballDir :: Dir,
    ballSpeed :: Float,
    paddlePos :: Pos,
    paddleMove :: Move,
    gameOver :: Bool, -- if lost, then TRUE
    score :: Int,
    scoreBoardShow :: Bool, -- when 'S' pressed before the game start, show the scoreboard
    gameStarted :: Bool -- has the player started playing (press 'P')?
  } deriving Show --in order to show player's score as a number