module GameIO where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game


-- a player controlls a paddle, bouncing a ball off to the wall above.
-- There are also borders right and left to the player
-- if he succesfully hits the ball off, he gets +1 score. 
-- if ball goes past the paddle, player looses.

--CONSTANTS

-- window size
windowWidth = 300
windowHeight = 300

-- wall
wallHeight = 10
wallWidth = windowWidth

-- paddle
paddleWidth = 2
paddleLength = 10
paddleSpeed = 10

--ball
ballRad = 10
ballSpeed = 5 -- MIGHT SPEED UP OVER THE GAME => INTO GameState???

fieldColor :: Color
fieldColor = green

ballColor :: Color
ballColor = white

paddleColor :: Color
paddleColor = brown

-- DATA/TYPES

type Pos = (Int, Int) -- player/ball position...
type Dir = (Int, Int) -- ...and direction

data Move = MoveRight | MoveLeft | NoMovement

data ScoreBoard = Scores {} -- a table of player's scores, storing top 5 up to date

data GameState = GS { 
  ballPos :: Pos,
  ballDir :: Dir,
  paddlePos :: Pos,
  paddleMove :: Move,
  score :: Int
} deriving Show -- want to be able to show player's score with a number

initialState :: GameState
initialState = GS (0, 0) (1, 1) (5,5) NoMovement 0

-- FUNCTIONS

run :: IO ()
run = do 
    gen <- getStdGen
    startGame gen
  
updateApp :: Float -> GameState -> GameState
-- running all the functions below in a row

startGame :: StdGen -> IO ()
-- startGame gen = play ... <gloss rendering specs using updateApp>

-- moving Paddle in one frame
movePaddle :: GameState -> Move -> GameState 
movePaddle gs MoveLeft = gs {paddlePos = (x - paddleSpeed, y)}
movePaddle gs MoveRight = gs {paddlePos = (x + paddleSpeed, y)}
movePaddle gs NoMove = gs

-- moving ball for some distance in ballDir direction
moveBall :: GameState -> Float -> GameState
moveBall gs dist = gs {ballPos = (x + dx, y + dy)} 

-- checking if ball collides with anything
checkWallCollision :: Pos -> Bool
checkPaddleCollision :: Pos -> Bool

-- functions using checks above to update the GS
wallHit :: GameState -> GameState
borderHit :: GameState -> GameState
paddleHit :: GameState -> GameState -- if TRUE, player's score +1!, other GS data kept the same
ballMissed :: GameState -> GameState -- if TRUE, player's score -1!, return to initial position

handleEvent :: Event -> GameState -> GameState
-- Handling the event: A - moving left, D - moving right
handleEvent (EventKey (Char 'a') Down _ _) game = game { paddleMove = MoveLeft }
handleEvent (EventKey (Char 'a') Up _ _) game = game { paddleMove = NoMovement }
handleEvent (EventKey (Char 'd') Down _ _) game = game { paddleMove = MoveRight }
handleEvent (EventKey (Char 'd') Up _ _) game = game { paddleMove = NoMovement }

-- RENDERING...