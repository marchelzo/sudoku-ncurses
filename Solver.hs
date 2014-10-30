module Solver where

import SudokuCore

import Control.Monad.State

type Coord = (Int, Int)

data SudokuSolverState = SudokuSolverState { board   :: Board
                                           , history :: ([Coord], Coord, [Coord])
                                           , done    :: Bool
                                           } deriving (Show)

solveSudoku :: Board -> Board
solveSudoku b = board (execState solve (initialState b))

initialState :: Board -> SudokuSolverState
initialState b = SudokuSolverState b ([], e, es) False
    where
        (e:es) = emptySquares b

solve :: State SudokuSolverState ()
solve = do
    (SudokuSolverState b _ _) <- get
    unless (solved b) $ do s <- attemptSolve
                           unless s $ do backtrack
                                         d <- liftM done get
                                         unless d $ do incrementCurrentSquare
                                                       solve
    return ()

backtrack :: State SudokuSolverState ()
backtrack = do (SudokuSolverState b _ _) <- get
               unless (validState b) $ do emptyCurrent
                                          decrementHistoryIndex
                                          backtrack
               (SudokuSolverState b' (_, (i,j), _) _) <- get
               when (valueAt i j b' == Full 9) $ do emptyCurrent
                                                    decrementHistoryIndex
                                                    d <- liftM done get
                                                    unless d backtrack


emptyCurrent :: State SudokuSolverState ()
emptyCurrent = do (SudokuSolverState b (_, (i,j), _) _) <- get
                  modify $ \s -> s { board = insertAt i j Empty b }

decrementHistoryIndex :: State SudokuSolverState ()
decrementHistoryIndex = do (SudokuSolverState _ (xs, x, ys) _) <- get
                           if null xs
                           then modify $ \s -> s { done = True }
                           else let (p:ps, c, fs) = (xs, x, ys)
                                    in modify $ \s -> s { history = (ps, p, c:fs) }

incrementHistoryIndex :: State SudokuSolverState ()
incrementHistoryIndex = do (SudokuSolverState _ (ps, c, f:fs) _) <- get
                           modify $ \s -> s { history = (c:ps, f, fs) }

incrementCurrentSquare :: State SudokuSolverState ()
incrementCurrentSquare = do (SudokuSolverState b (_, (i,j), _) _) <- get
                            modify $ \s -> s { board = incrementAt i j b }

solveCurrentSquare' :: State SudokuSolverState Bool
solveCurrentSquare' = do (SudokuSolverState b (_, (i,j), _) _) <- get
                         let validNumbers = [x | x <- [1..9], validMove i j x b]
                         case validNumbers of
                             []    -> return False
                             (x:_) -> do modify $ \s -> s { board = insertAt i j (Full x) b }
                                         return True

solveCurrentSquare :: State SudokuSolverState Bool
solveCurrentSquare = do (SudokuSolverState b (_, (i,j), _) _) <- get
                        if (not . validState) b && (valueAt i j b == Full 9)
                        then return False
                        else if valueAt i j b /= Empty && validState b
                             then return True
                             else do incrementCurrentSquare
                                     solveCurrentSquare

isSolved :: State SudokuSolverState Bool
isSolved = do (SudokuSolverState b _ _) <- get
              return (solved b)

attemptSolve :: State SudokuSolverState Bool
attemptSolve = do s <- isSolved
                  if s
                  then return True
                  else do c <- solveCurrentSquare
                          if c
                          then do s' <- isSolved
                                  if s' then return True
                                  else do incrementHistoryIndex
                                          attemptSolve
                          else return False


onLastSquare :: State SudokuSolverState Bool
onLastSquare = do (SudokuSolverState _ (_, _, sqs) _) <- get
                  return (null sqs)


setAt :: Int -> Int -> Square -> State SudokuSolverState ()
setAt i j sq = do b <- liftM board get
                  modify $ \s -> s { board = insertAt i j sq b }
