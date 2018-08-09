module Lib
    ( formatGrid
    , outputGrid
    , findWord
    , findWords
    , findWordInLine
    , skew
    , zipOverGrid
    , zipOverGridWith
    , coordsGrid
    , gridWithCoords
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

data Cell = Cell (Integer, Integer) Char deriving (Eq, Ord, Show)
type Grid a = [[a]]

zipOverGrid :: Grid a -> Grid b -> Grid (a, b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

coordsGrid :: Grid (Integer, Integer)
coordsGrid =
    let rows = map repeat [0..]
        cols = repeat [0..]
    in zipOverGrid rows cols

gridWithCoords grid = zipOverGridWith Cell coordsGrid grid

outputGrid :: Grid Char -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid Char -> String
formatGrid = unlines

getLines :: Grid Char -> [String]
getLines grid =
    let horizontal = grid
        vertical = transpose grid
        diagonalFront = diagonalize grid
        diagonalBack = diagonalize (map reverse grid)
        lines = horizontal ++ vertical ++ diagonalFront ++ diagonalBack
    in lines ++ (map reverse lines)

diagonalize :: Grid Char -> Grid Char
diagonalize = transpose . skew

skew :: Grid Char -> Grid Char
skew [] = [] 
skew (l:ls) = l : skew (map indent ls)
    where indent line = '_' : line

findWord :: Grid Char -> String -> Maybe String
findWord grid word =
    let lines = getLines grid
        found = or $ map (findWordInLine word) lines
    in if found then Just word else Nothing

findWords :: Grid Char -> [String] -> [String]
findWords grid words =
    let foundWords = map (findWord grid) words
    in catMaybes foundWords

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf