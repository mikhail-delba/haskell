module GameIO where

import Data.Map
import Data.Set as S
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.Random.Shuffle (shuffle')


--REVAMP
--insert

--REWRITE:
--viewPort
--rendering with bitmaps

--don't explode on first step???
--viewport

--STILL CANNOT WIN ALTHOUGH CAN LOOSE :)

run :: IO ()
run = do
    gen <- getStdGen
    startGame gen

fieldSize@(fieldWidth, fieldHeight) = (10, 10) :: (Int, Int)
mineCount = 5 :: Int

createField :: Field
createField = Data.Map.empty

type Field = Map Cell CellState
type Cell = (Int, Int)

data CellState = Opened Int -- opened the cell; the parameter is the number of mined neighbor cells
               | Mine       -- mined cell
               | Flag       -- flag placed

type Mines = Set Cell

-- generating the set of mines using shuffle function

createMines :: RandomGen g => g -> Cell -> Mines
createMines g fst = S.fromList $ Prelude.take mineCount $ shuffle g $ [(i, j)  | i <- [0 .. fieldWidth - 1] 
              , j <- [0 .. fieldHeight - 1]
              , (i, j) /= fst]

shuffle g l = shuffle' l (fieldWidth * fieldHeight - 1) g

data GameState = GS
    { field    :: Field
    , mines    :: Either StdGen Mines
    , gameOver :: Bool
    }

startGame :: StdGen -> IO ()
startGame gen = play (InWindow "DANGER MINES" windowSize (240, 160)) white 30 (initState gen) renderer handler updater

-- additional function to perform function with both tuple members
twoFunc :: (a -> b) -> (a, a) -> (b, b)
twoFunc f (a, b) = (f a, f b)

windowSize = twoFunc (* round cellSize) fieldSize
cellSize = 24 :: Float

initState gen = GS createField (Left gen) False

updater _ = id

cellToScreen = twoFunc ((* cellSize) . fromIntegral)

-- first click on the scene => generating the mines
handler (EventKey (MouseButton LeftButton) Down _ mouse) gs@GS 
    { 
      mines = Left gen
    } = gs 
        { 
          mines = Right $ createMines gen cell 
        } where cell = screenToCell mouse

handler (EventKey (MouseButton LeftButton) Down _ mouse) gs@GS
    {
      field = field
    , mines = Right mines
    , gameOver = False
    } = gs
    { 
      field = newField
    , gameOver = exploded -- game over if exploded
    } where
    newField = click cell field

    exploded = case Data.Map.lookup cell newField of -- explode if last checked cell is a mine
        Just Mine -> True
        _         -> False

    -- determine the cell player clicks on
    cell@(cx, cy) = screenToCell mouse
    
    click :: Cell -> Field -> Field
    click cell@(cx, cy) field
        | cell `Data.Map.member` field = field -- do nothing if cell already clicked
        | cell `S.member` mines = Data.Map.insert cell Mine field -- stepped on a mine
        | otherwise = Data.Map.insert cell(Opened neighbours) field
        where
            -- nearby cells
            neighbourCells = [ (i, j) | i <- [cx - 1 .. cx + 1], 0 <= i && i < fieldWidth , j <- [cy - 1 .. cy + 1], 0 <= j && j < fieldHeight] 
            -- check if neighbours are mines and return their num
            neighbours = length $ Prelude.filter (`S.member` mines) neighbourCells


handler (EventKey (MouseButton RightButton) Down _ mouse) gs@GS { 
    field = field
    } = case Data.Map.lookup coord field of
        Nothing -> gs { field = Data.Map.insert coord Flag field }
        Just Flag -> gs { field = Data.Map.delete coord field }
        _ -> gs
        where coord = screenToCell mouse
handler _ gs = gs

-- coords translation: screen to cell
screenToCell = twoFunc (round . (/ cellSize)) . invertViewPort viewPort



--- Rendering 

-- using ViewPort and Picture classes from Gloss to render
viewPort = ViewPort (twoFunc ((* (-1)) . (/ 2) . subtract cellSize) $ cellToScreen fieldSize) 0 1

-- function for placing a label with black default text
label = translate (-5) (-5) . scale 0.1 0.1 . color black . text

renderer GS { field = field } = applyViewPortToPicture viewPort $ pictures $ cells ++ grid where
    grid = [uncurry translate (cellToScreen (x, y)) $ color black $ rectangleWire cellSize cellSize | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1]]
    cells = [uncurry translate (cellToScreen (x, y)) $ drawCell x y | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1]]
    
    drawCell x y = case Data.Map.lookup (x, y) field of -- looking up for the cell's coords in the field set...

        -- depending on the CellState of the looked up cell:
        Nothing         -> color white $ rectangleSolid cellSize cellSize -- draw empty unvisited cell
        Just Mine       -> pictures [ color black $ circleSolid (cellSize/2.5)] -- draw mine
        Just (Opened 0) ->  pictures [ color green $ rectangleSolid cellSize cellSize] -- draw an opened cell with 0 mined neighbors 
        Just (Opened n) -> pictures [ color green $ rectangleSolid cellSize cellSize, label $ show n] -- draw an opened cell with a num of mined neighbour
        Just Flag       -> pictures [ color white $ rectangleSolid cellSize cellSize, label "V"] -- pin a 'V' flag on a cell