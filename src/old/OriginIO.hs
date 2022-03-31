module OriginIO where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

import Data.Set -- lib for creating sets, f.e. set of Mines


-- CONSTANTS

mineNum :: Int
mineNum = 10

fieldSize :: (Int, Int)
fieldSize = (20,20) 


-- TYPE CONSTRUCTORS

type Cell = (Int, Int) -- type: a field cell, consists of two cords

type MineSet = Set Cell-- type: list of mines

type Field = Set Cell CellState -- field, might be some grid from gloss?


-- DATA CONSTRUCTORS

-- Cell state

data CellState = Checked -- Checked cell, either with a mine or not
                | Mine   -- Cell explodes -> game over
                | Flag   -- Flag placed on cell
                | Int    -- cell num (proximity)
                | Empty

-- Game state
data AppState = AppState
    { field :: Field -- the game field
    , mines_set :: MineSet -- the mines
    , gameOver :: Bool 
    -- scoreboard
    }

data Scoreboard = Scores {}

-- FUNCTIONS/SIGNATURES

-- creating the game's field
generateField :: Field -> MineSet -- RandomGen -> AppState

-- spawn mines on the field
spawnMines :: RandomGen -> MineSet

-- Simulation step (updates nothing).
updateApp :: Float -> AppState -> AppState -- Event ->...
updateApp _ x = x

-- STARTING GAME
startGame :: RandomGen -> IO () -- Settings...


-- RUNNING GAME
run :: IO ()
run = do
   -- rndGen <- newStdGen ??
   -- startGame rndGen ??


-- HANDLE EVENTS

handleEvent :: Event -> AppState -> AppState
-- Handling the event: mouse left button clicked...
handleEvent (EventKey (MouseButton LeftButton) Down _ _) state -- check cell
handleEvent (EventKey (MouseButton RightButton) Down _ _) state -- place flag on a cell