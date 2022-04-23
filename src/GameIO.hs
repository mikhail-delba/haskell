module GameIO where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Database.SQLite.Simple
import System.Environment
--import Text.Read

import Consts
import Physics
import Rendering
import Table

-- game state updater -- 
updateApp :: Float -> GameState -> IO GameState
updateApp _ gs@GS{gameOver = True} = do 
                                      return gs
updateApp t gs = return (updateGS t gs)


updateGS :: Float  -> GameState -> GameState
updateGS _ gs@GS{gameStarted = False} = gs
updateGS t gs = ballMissed . physicsCollision . paddleControl . physicsBall t $ gs


checkArgs :: [String] -> String
checkArgs [name] = name
checkArgs _ = "Mike"

-- add random direction and location
startGame :: IO () -- enter username here
startGame = do
            cmdArgs <- getArgs
            conn <- open "scoreboard.db"
            scores <- getTop5 conn
            playIO (InWindow "Ping-Pong" (windowWidth, windowHeight) (500, 200)) 
                          fieldColor 60 (initialState conn (checkArgs cmdArgs) scores) 
                            render handleEventStart updateApp
            close conn

-- EVENT HANDLING --
-- scoreboard and game start controls

handleEventStart :: Event -> GameState -> IO GameState
handleEventStart (EventKey (Graphics.Gloss.Interface.IO.Game.Char 'p') Down _ _) gs@GS{
  gameStarted = False} = if gameOver gs then do
                                              createID (userName gs)
                                              writeScoreToTable (score gs) (connection gs)
                                              return (gs { gameStarted = True,
                                              scoreBoardShow = False, gameOver = False, score = 0})
                                            else 
                                              return (gs { gameStarted = True,
                                              scoreBoardShow = False, score = 0})
                                              
handleEventStart event gs = return (handleEvent event gs)

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (Graphics.Gloss.Interface.IO.Game.Char 's') Down _ _) gs@GS{
  gameStarted = False, scoreBoardShow = False} = gs {scoreBoardShow = True}
handleEvent (EventKey (Graphics.Gloss.Interface.IO.Game.Char 's') Down _ _) gs@GS{
  gameStarted = False, scoreBoardShow = True} = gs {scoreBoardShow = False} 
-- showing the scoreboard (S)
handleEvent (EventKey (Graphics.Gloss.Interface.IO.Game.Char 'a') Down _ _) gs = gs { 
  paddleMove = MoveLeft }
handleEvent (EventKey (Graphics.Gloss.Interface.IO.Game.Char 'a') Up _ _) gs = gs { 
  paddleMove = NoMovement }
handleEvent (EventKey (Graphics.Gloss.Interface.IO.Game.Char 'd') Down _ _) gs = gs {
   paddleMove = MoveRight }
handleEvent (EventKey (Graphics.Gloss.Interface.IO.Game.Char 'd') Up _ _) gs = gs { 
  paddleMove = NoMovement }
handleEvent _ gs = gs