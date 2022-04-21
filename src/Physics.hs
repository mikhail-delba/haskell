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
    checkMaxX (px,py) = (max (-paddleMaxX) (min paddleMaxX px), py)

    -- depending on the input, moving the paddle --
    paddleMovePos :: Pos -> Move -> Pos
    paddleMovePos (px, py) MoveLeft = (px - paddleSpeed, py)
    paddleMovePos (px, py) MoveRight = (px + paddleSpeed, py)
    paddleMovePos pos NoMovement = pos

-- check if the ball collided with a paddle...
checkPaddleCollisionV1 :: Pos -> Pos -> Bool
checkPaddleCollisionV1 (bx, by) (px, py) = checkX && checkY -- check if the ball falls for both axes
  where
    checkX = (bx - ballRad <= px + paddleLength) && (bx - ballRad >= px - paddleLength)
    checkY = (by - ballRad >= py - paddleWidth) && (by - ballRad <= py + paddleWidth)

checkPaddleCollision :: Pos -> Pos -> Bool
checkPaddleCollision (bx, by) (px, py) = (dX ** 2 + dY ** 2) < (ballRad ** 2)
-- check if the ball collides with the paddle
  where
    cornerX = px - paddleLength
    cornerY = py - paddleWidth
    dX = bx - max cornerX (min bx (cornerX + paddleLength * 2))
    dY = by - max cornerY (min by (cornerY + paddleWidth * 2))
  
-- check if the ball collided with the upper wall
checkWallCollision :: Pos -> Bool
checkWallCollision (_, py) =  py + ballRad >= fromIntegral windowWidth / 2

-- check if the ball collided with either border
checkBorderCollision :: Pos -> Bool
checkBorderCollision (x, _) = leftBorderCollision || rightBorderCollision
  where
    leftBorderCollision = x - ballRad <= -fromIntegral windowWidth / 2
    rightBorderCollision = x + ballRad >= fromIntegral windowWidth / 2

-- the overall function that checks any possible collision & hit
physicsCollision :: GameState -> GameState
physicsCollision = borderHit . wallHit . paddleHit
    where 
    paddleHit :: GameState -> GameState
    paddleHit gs@GS {ballDir = (x, y), ballPos = (bx,_), paddlePos = (px,_)} =  gs { ballDir = (x', y'), 
                                  ballSpeed = speed, score = scoreValue}
      where
        collided = checkPaddleCollision (ballPos gs) (paddlePos gs)
        rightSide = bx > px + paddleLength
        leftSide = bx < px - paddleLength
   
        y' = if collided && (y < 0) then -y else y -- top collision => y vector changes
        x' = if collided && ((x<0) && rightSide || (x>0) && leftSide) then -x else x -- side collision => x vector changes

        bounceOff = collided && ( (y<0) || ((x<0) && rightSide || (x>0) && leftSide))

        speed = if bounceOff
             then ballSpeed gs + 10 -- ball speed + 10 when hit by a paddle
             else ballSpeed gs
        scoreValue = if bounceOff      
              then score gs + 1 else score gs
    
    wallHit :: GameState -> GameState
    wallHit gs@GS {ballPos = (px, py), ballDir = (x, y)} = gs { ballDir = (x, y') }
      where y' = if checkWallCollision (px, py) then -y else y

    borderHit :: GameState -> GameState
    borderHit gs@GS {ballPos = (px, py), ballDir = (x, y)} = gs { ballDir = (x', y) }
      where x' = if checkBorderCollision (px, py) then -x else x

-- if the ball fell behind the paddle...
ballMissed :: GameState -> GameState
ballMissed gs@GS {ballPos = (_, y)} = if y <= - fromIntegral windowHeight/2 
                                        then gs { ballPos = initBallPos, ballSpeed = initSpeed,
                                         paddlePos = initPaddlePos, 
                                         gameStarted = False, 
                                         score = 0,
                                         gameOver = True 
                                         } -- lost => gameOver = TRUE
                                      else gs