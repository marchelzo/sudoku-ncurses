module Solver where

import SudokuCore

import Control.Monad.State

type Coord = (Int, Int)

data SudokuSolverState = SudokuSolverState { board   :: Board
                                           , history :: ([Coord], Coord, [Coord])
                                           , num     :: Int
                                           } deriving (Show)

initialState :: Board -> SudokuSolverState
initialState b = SudokuSolverState b ([], e, es) 0
    where
        (e:es) = emptySquares b

solve :: State SudokuSolverState ()
solve = do
    (SudokuSolverState b h n) <- get
    return ()

backtrack :: State SudokuSolverState ()
backtrack = do (SudokuSolverState b h n) <- get
               unless (validState b) $ do emptyCurrent
                                          decrementHistoryIndex
                                          backtrack

emptyCurrent :: State SudokuSolverState ()
emptyCurrent = do (SudokuSolverState b (_, (i,j), _) _) <- get
                  modify $ \s -> s { board = (insertAt i j Empty b) }

decrementHistoryIndex :: State SudokuSolverState ()
decrementHistoryIndex = do (SudokuSolverState _ ((p:ps), c, fs) _) <- get
                           modify $ \s -> s { history = (ps, p, (c:fs)) }

incrementHistoryIndex :: State SudokuSolverState ()
incrementHistoryIndex = do (SudokuSolverState _ (ps, c, (f:fs)) _) <- get
                           modify $ \s -> s { history = ((c:ps), f, fs) }

incrementCurrentSquare :: State SudokuSolverState ()
incrementCurrentSquare = do (SudokuSolverState b (_, (i,j), _) _) <- get
                            modify $ \s -> s { board = incrementAt i j b }
