module GameIO where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Consts
import Physics
import Rendering

run :: IO ()
run = do 
    gen <- getStdGen
    startGame gen

-- game state updater -- 
updateApp :: Float -> GameState -> GameState
updateApp t = ballMissed . physicsCollision . paddleControl . physicsBall t -- check gameOver here???

-- add random direction and location
startGame :: StdGen -> IO ()
startGame gen = play (InWindow "Ping-Pong" (windowWidth, windowHeight) (500, 200)) 
                          fieldColor 60 initialState render handleEvent updateApp

-- EVENT HANDLING --
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (Char 'a') Down _ _) gs = gs { paddleMove = MoveLeft }
handleEvent (EventKey (Char 'a') Up _ _) gs = gs { paddleMove = NoMovement }
handleEvent (EventKey (Char 'd') Down _ _) gs = gs { paddleMove = MoveRight }
handleEvent (EventKey (Char 'd') Up _ _) gs = gs { paddleMove = NoMovement }
handleEvent _ gs = gs