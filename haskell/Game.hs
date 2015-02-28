module Game where

import           Data.List
import           Data.Maybe

data Status = Dead | Alive deriving (Show, Eq)

type GridSize = (Int, Int)
type Grid = [[Status]]
type Cell = (Int, Int)

getGridSize :: Grid -> GridSize
getGridSize g = (length g, length (head g))

int2Status :: Int -> Status
int2Status 0 = Dead
int2Status 1 = Alive
int2Status _ = undefined

ints2Status :: [Int] -> [Status]
ints2Status = map int2Status

createGrid :: [[Int]] -> Grid
createGrid = map ints2Status

getCellStatus :: Cell -> Grid -> Maybe Status
getCellStatus (row, column) g
	| isValidCoordinate (row, column) g = Just $ g !! row !! column
	| otherwise = Nothing

flipCellStatus :: Status -> Status
flipCellStatus Dead = Alive
flipCellStatus Alive = Dead

invertGrid :: Grid -> Grid
invertGrid = map $ map flipCellStatus

isValidCoordinate :: Cell -> Grid -> Bool
isValidCoordinate (row, column) g =
	and $ [row < gridRow, column < gridColumn, row >= 0, column >= 0]
	where (gridRow, gridColumn) = getGridSize g

getNeighbours :: Cell -> Grid -> [Cell]
getNeighbours (row, column) g = filter ((flip isValidCoordinate) g) [
	(row - 1, column - 1),
	(row - 1, column    ),
	(row - 1, column + 1),
	(row    , column - 1),
	(row    , column + 1),
	(row + 1, column - 1),
	(row + 1, column    ),
	(row + 1, column + 1)
	]

getAliveCount :: Grid -> [Cell] -> Int
getAliveCount grid = length . (filter (== Alive)) . map fromJust . map ((flip getCellStatus) grid)

getAliveNeighbourCount :: Cell -> Grid -> Int
getAliveNeighbourCount c g = getAliveCount g $ getNeighbours c g

getNextStatus :: Status -> Int -> Status
getNextStatus Dead liveNeighbours
	| liveNeighbours == 3 = Alive
	| otherwise           = Dead
getNextStatus Alive liveNeighbours
	| liveNeighbours < 2 = Dead
	| liveNeighbours > 3 = Dead
	| otherwise          = Alive

getNextCellStatus :: Cell -> Grid -> Status
getNextCellStatus c g = getNextStatus (fromJust (getCellStatus c g)) (getAliveNeighbourCount c g)

getCellList :: Grid -> [[Cell]]
getCellList g = [[(row, col) | col <- [0..cols - 1]] | row <- [0..rows - 1]]
	where (rows, cols) = getGridSize g

getNextGeneration :: Grid -> Grid
getNextGeneration g = map (map ((flip getNextCellStatus) g)) cellList
	where cellList = getCellList g

