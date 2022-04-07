module GameIO where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

-- a player controlls a paddle, bouncing a ball off to the wall above.
-- There are also borders right and left to the player
-- if he succesfully hits the ball off, he gets +1 score. 
-- if the ball goes past the paddle, player looses.
-- the ball increases it spead when it is hit
-- gameOver mechanic not implemented yet, headed for scoreboard

-- CONSTANTS --

-- window size
windowWidth, windowHeight :: Int
windowWidth = 500
windowHeight = 500

paddleMaxX = (fromIntegral windowWidth/2) - paddleLength - wallWidth

-- wall
wallWidth = 10

-- paddle
paddleWidth = 10
paddleLength = 50
paddleSpeed = 10

-- ball
ballRad = 10

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
    score :: Int
  } deriving Show --in order to show player's score as a number

-- Initial values --
initBallPos = (0.0, 0.0)
initDir = (1.0, 1.0)
initSpeed = 200.0
initPaddlePos = (0.0, -150.0)

initialState :: GameState
initialState = GS initBallPos initDir initSpeed initPaddlePos NoMovement False 0

run :: IO ()
run = do 
    gen <- getStdGen
    startGame gen

-- game state updater -- 
updateApp :: Float -> GameState -> GameState
updateApp t = ballMissed . physicsCollision . paddleControl . physicsBall t -- check gameOver here???

startGame :: StdGen -> IO ()
startGame gen = play (InWindow "Ping-Pong" (windowWidth, windowHeight) (500, 200)) 
                          fieldColor 60 initialState render handleEvent updateApp

-- PHYSICS --

-- calculating ball's motion based on current position and direction
physicsBall :: Float -> GameState -> GameState
physicsBall t gs@GS {ballPos = (x, y), ballDir = (dx, dy)} = gs { ballPos = (x', y') }
  where
    x' = x + dx * t * (ballSpeed gs)
    y' = y + dy * t * (ballSpeed gs)

paddleControl :: GameState -> GameState
paddleControl gs@GS {paddlePos = (x, y), paddleMove = move} = gs { 
                                   paddlePos = checkMaxX $ paddleMovePos (x,y) move }
  where 
    checkMaxX :: Pos -> Pos
    checkMaxX (x, y) = (max (-paddleMaxX) (min paddleMaxX x), y)

    -- depending on the input, moving the paddle --
    paddleMovePos :: Pos -> Move -> Pos
    paddleMovePos (x, y) MoveLeft = (x - paddleSpeed, y)
    paddleMovePos (x, y) MoveRight = (x + paddleSpeed, y)
    paddleMovePos pos NoMovement = pos

-- check if the ball collided with a paddle...
checkPaddleCollision :: Pos -> Pos -> Bool
checkPaddleCollision (bx, by) (px, py) = checkX && checkY -- check if the ball falls for both axes
  where
    checkX = (bx - ballRad <= px + paddleLength) && (bx - ballRad >= px - paddleLength)
    checkY = (by - ballRad>= py - paddleWidth) && (by - ballRad <= py + paddleWidth)

-- check if the ball collided with the upper wall
checkWallCollision :: Pos -> Bool
checkWallCollision (x, y) =  y + ballRad >= fromIntegral windowWidth / 2

-- check if the ball collided with either border
checkBorderCollision :: Pos -> Bool
checkBorderCollision (x, y) = leftBorderCollision || rightBorderCollision
  where
    leftBorderCollision = x - ballRad <= -fromIntegral windowWidth / 2
    rightBorderCollision = x + ballRad >= fromIntegral windowWidth / 2

-- the overall function that checks any possible collision & hit
physicsCollision :: GameState -> GameState
physicsCollision = borderHit . wallHit . paddleHit
    where 
    paddleHit :: GameState -> GameState
    paddleHit gs@GS {ballDir = (x, y)} = gs { ballDir = (x, y'), ballSpeed = speed, score = scoreValue}
      where
        collided = checkPaddleCollision (ballPos gs) (paddlePos gs)
        y' = if collided then -y else y
        speed = if collided
             then ballSpeed gs + 10 -- ball speed + 10 when hit by a paddle
             else ballSpeed gs
        scoreValue = if collided
              then score gs + 1 else score gs
    
    wallHit :: GameState -> GameState
    wallHit gs@GS {ballPos = (px, py), ballDir = (x, y)} = gs { ballDir = (x, y') }
      where y' = if checkWallCollision (px, py) then -y else y

    borderHit :: GameState -> GameState
    borderHit gs@GS {ballPos = (px, py), ballDir = (x, y)} = gs { ballDir = (x', y) }
      where x' = if checkBorderCollision (px, py) then -x else x

-- if the ball fell behind the paddle...
ballMissed :: GameState -> GameState
ballMissed gs@GS {ballPos = (x, y)} = if y <= - fromIntegral windowHeight/2 
                                        then gs { ballPos = initBallPos, ballSpeed = initSpeed,
                                         paddlePos = initPaddlePos, 
                                         gameOver = True, 
                                         score = 0 } -- lost => gameOver = TRUE
                                      else gs

-- EVENT HANDLING --
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (Char 'a') Down _ _) gs = gs { paddleMove = MoveLeft }
handleEvent (EventKey (Char 'a') Up _ _) gs = gs { paddleMove = NoMovement }
handleEvent (EventKey (Char 'd') Down _ _) gs = gs { paddleMove = MoveRight }
handleEvent (EventKey (Char 'd') Up _ _) gs = gs { paddleMove = NoMovement }
handleEvent _ gs = gs


-- RENDERING --
drawPaddle :: Pos -> Picture
drawPaddle (x, y) = pictures [ translate x y $ 
      color paddleColor $
       rectangleSolid (2 * paddleLength) (2 * paddleWidth)]

drawBorder :: Float -> Picture
drawBorder dist = translate dist 0 $ color borderColor $ 
                  rectangleSolid wallWidth (fromIntegral windowHeight)

render :: GameState -> Picture
render gs@GS {ballPos = (x, y), paddlePos = (px, py)} = pictures [ballPic, drawPaddle (px, py), 
                                                              wallPic, borderPics, scoreText] -- rendering a list of pictures translated below
  where
    ballPic = translate x y $ color ballColor $ circleSolid ballRad
    
    --upper wall
    wallPic = translate 0 (fromIntegral windowWidth/2) $ color wallColor $ 
                      rectangleSolid (fromIntegral windowWidth) wallWidth
    --side borders
    borderPics = pictures [drawBorder (-fromIntegral windowWidth/2), 
                        drawBorder (fromIntegral windowWidth/2)]

    scoreText = translate (-50) 150 $
      scale 0.2 0.2 $ color white $ text ("SCORE: " ++ show (score gs))