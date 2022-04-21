module GameIO where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Database.SQLite.Simple

import Consts
import Physics
import Rendering
import Table

run :: IO ()
run = do 
    startGame

-- game state updater -- 
updateApp :: Float -> GameState -> IO GameState
updateApp t gs = if (gameStarted gs) then return (updateGS t gs) else return gs

updateGS :: Float  -> GameState -> GameState
updateGS t = ballMissed . physicsCollision . paddleControl . physicsBall t


-- add random direction and location
startGame :: IO () -- enter username here
startGame = do
            conn <- open "scoreboard.db"
            scores <- getTop5 conn
           -- createID "Vladimir"
            playIO (InWindow "Ping-Pong" (windowWidth, windowHeight) (500, 200)) 
                          fieldColor 60 (initialState conn scores) render handleEvent updateApp
            close conn

-- EVENT HANDLING --
-- scoreboard and game start controls



handleEvent :: Event -> GameState -> IO GameState
handleEvent (EventKey (Char 'p') Down _ _) gs = if (gameOver gs) then do
                                              --writeScoreToTable (score gs) (connection gs)
                                              return (gs{ gameStarted = True, scoreBoardShow = False, gameOver = False, score = 0})
                                            else 
                                              return (gs { gameStarted = True, scoreBoardShow = False, gameOver = False, score = 0})
handleEvent (EventKey (Char 's') Down _ _) gs@GS{gameStarted = False, scoreBoardShow = False} = return gs {scoreBoardShow = True} -- showing the scoreboard (S)
handleEvent (EventKey (Char 's') Down _ _) gs@GS{gameStarted = False, scoreBoardShow = True} = return gs {scoreBoardShow = False} -- showing the scoreboard (S)
handleEvent (EventKey (Char 'a') Down _ _) gs = return gs { paddleMove = MoveLeft }
handleEvent (EventKey (Char 'a') Up _ _) gs = return gs { paddleMove = NoMovement }
handleEvent (EventKey (Char 'd') Down _ _) gs = return gs { paddleMove = MoveRight }
handleEvent (EventKey (Char 'd') Up _ _) gs = return gs { paddleMove = NoMovement }
handleEvent _ gs = return gs