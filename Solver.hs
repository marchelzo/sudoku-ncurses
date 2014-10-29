{-# LANGUAGE TemplateHaskell #-}

module Solver where

import SudokuCore

import Control.Monad.State
import Control.Lens

data SudokuSolverState = SudokuSolverState { _board   :: Board
                                           , _history :: [(Int,Int)]
                                           , _num     :: Int
                                           } deriving (Show)

makeLenses ''SudokuSolverState

initialState :: Board -> SudokuSolverState
initialState b = SudokuSolverState b [] 0

solve :: State SudokuSolverState ()
solve = do
    (SudokuSolverState b h n) <- get
    let (i,j) = firstEmpty b
    
    return ()
