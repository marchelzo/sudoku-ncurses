module Sudoku.Core where

import Data.List (transpose)

data Square = Full Int | Empty

data Board = Board [[Square]]

startingBoard :: Board
startingBoard = Board [ [Full 1,Full 2,Full 3,Full 4,Full 5,Full 6,Full 7,Full 8,Full 9]
                      , [Full 4,Full 5,Full 6,Full 7,Full 8,Full 9,Full 1,Full 2,Full 3]
                      , [Full 7,Full 8,Full 9,Full 1,Full 2,Full 3,Full 4,Full 5,Full 6]
                      , [Full 2,Full 3,Full 4,Full 5,Full 6,Full 7,Full 8,Full 9,Full 1]
                      , [Full 5,Full 6,Full 7,Full 8,Full 9,Full 1,Full 2,Full 3,Full 4]
                      , [Full 8,Full 9,Full 1,Full 2,Full 3,Full 4,Full 5,Full 6,Full 7]
                      , [Full 3,Full 4,Full 5,Full 6,Full 7,Full 8,Full 9,Full 1,Full 2]
                      , [Full 6,Full 7,Full 8,Full 9,Full 1,Full 2,Full 3,Full 4,Full 5]
                      , [Full 9,Full 1,Full 2,Full 3,Full 4,Full 5,Full 6,Full 7,Full 8]
                      ]

instance Show Board where
    show (Board b) = concatMap ((++"\n") . showRow) b
        where
            showRow = unwords . map show

instance Show Square where
    show (Full n) = show n
    show _        = " "

-- | Functions for manupulating Sudoku boards while maintaining a solved state

reflectMajor :: Board -> Board
reflectMajor (Board b) = Board (transpose b)

reflectMinor :: Board -> Board
reflectMinor (Board b) = (Board . map reverse . transpose . map reverse) b
