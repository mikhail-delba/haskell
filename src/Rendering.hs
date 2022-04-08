module Rendering where

import Graphics.Gloss

import Consts

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