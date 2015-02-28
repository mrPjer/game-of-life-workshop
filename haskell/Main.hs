module Main where

import           Game
import           Data.Char
import           Control.Monad
import           Data.List
import           System.Posix.Unistd

field = createGrid [
	[0, 0, 1, 0, 0, 0],
	[1, 0, 1, 0, 0, 0],
	[0, 1, 1, 0, 0, 0],
	[0, 0, 0, 0, 0, 0],
	[0, 0, 0, 0, 0, 0],
	[0, 0, 0, 0, 0, 0]
	]

liveSymbol = 'X'
deadSymbol = '.'

showStatus :: Status -> Char
showStatus Dead = deadSymbol
showStatus Alive = liveSymbol

toDisplay :: Grid -> [[Char]]
toDisplay = map $ map showStatus

run table = do
	putStrLn $ intercalate "\n" $ toDisplay table
	sleep 1
	putStrLn ('\27' : "[2J")
	run (getNextGeneration table)

g1 = getNextGeneration field
g2 = getNextGeneration g1
g3 = getNextGeneration g2
g4 = getNextGeneration g3

main = run field
