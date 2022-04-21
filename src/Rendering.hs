module Rendering where

import Graphics.Gloss

import Consts
--import Table

-- RENDERING --
drawPaddle :: Pos -> Picture
drawPaddle (x, y) = pictures [ translate x y $ 
      color paddleColor $
       rectangleSolid (2 * paddleLength) (2 * paddleWidth)]

drawBorder :: Float -> Picture
drawBorder dist = translate dist 0 $ color borderColor $ 
                  rectangleSolid wallWidth (fromIntegral windowHeight)

render :: GameState -> IO Picture
 -- rendering a list of pictures translated below
render gs@GS {ballPos = (x, y), paddlePos = (px, py)} = return (pictures [ballPic, drawPaddle (px, py), 
                                                              wallPic, borderPics, scoreText, centerBall, cornerBall, lostSign, pictures (drawScoreBoard (-50) 180 gs)] )
  where
    ballPic = translate x y $ color ballColor $ circleSolid ballRad
    
    --upper wall
    wallPic = translate 0 (fromIntegral windowWidth/2) $ color wallColor $ 
                      rectangleSolid (fromIntegral windowWidth) wallWidth
    --side borders
    borderPics = pictures [drawBorder (-fromIntegral windowWidth/2), 
                        drawBorder (fromIntegral windowWidth/2)]

    scoreText = if gameStarted gs then translate (-50) 150 $
      scale 0.2 0.2 $ color white $ text ("SCORE: " ++ show (score gs))
                  else Blank
    
    centerBall = translate px py $ color ballColor $ circleSolid 2 -- used for debug

    cornerBall = translate (px - paddleLength) (py - paddleWidth) $ color red $ circleSolid 2 -- used for debug

    lostSign =  if gameOver gs then translate (-80) 100 $
      scale 0.5 0.5 $ color black $ text ("LOST")
                  else Blank



drawScoreBoard ::Int -> Int -> GameState -> [Picture] -- start from coords, drawing the text lines below each other

drawScoreBoard x y gs = if scoreBoardShow gs then showRow x y (scoresList gs)
  else [Blank]
    
showRow :: Int -> Int -> [(String, Int)] -> [Picture]
showRow x' y' ((name, num) : xs) = [translate (fromIntegral x') (fromIntegral (y'-20)) $ scale 0.15 0.15 $ color white $ text (name ++ show num)] ++ (showRow x' (y'-20) xs)
showRow _ _ [] = [Blank]
