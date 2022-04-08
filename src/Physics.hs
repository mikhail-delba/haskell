module Physics where

import Consts

-- PHYSICS --

-- calculating ball's motion based on current position and direction
physicsBall :: Float -> GameState -> GameState
physicsBall t gs@GS {ballPos = (x, y), ballDir = (dx, dy)} = gs { ballPos = (x', y') }
  where
    x' = x + dx * t * ballSpeed gs
    y' = y + dy * t * ballSpeed gs

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
    paddleHit gs@GS {ballDir = (x, y)} = gs { ballDir = (x, y'), 
                                  ballSpeed = speed, score = scoreValue}
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
