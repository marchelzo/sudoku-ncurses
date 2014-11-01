module SudokuCore where

import System.Random
import Control.Monad (replicateM)
import Data.List (sort, transpose, nub)
import Foreign.C

data Square = Full Int | Empty deriving (Eq)

data Board = Board [[Square]]

data Transformation = Rotate
                    | Swap Int Int
                    | ReflectMajor
                    | ReflectMinor
                    deriving (Show)

instance Random Transformation where
    random g = case (randomR :: RandomGen g => (Int,Int) -> g -> (Int,g)) (0,3) g of
                   (t, g') -> case t of
                                  0 -> (Rotate, g')
                                  1 -> let (x,y,g'') = twoRandomInts g' in (Swap x y, g'')
                                  2 -> (ReflectMajor, g')
                                  3 -> (ReflectMinor, g')
                   where
                       twoRandomInts g = let (x,g') = randomR (1,9) g
                                             in let (y,g'') = randomR (1,9) g'
                                                    in (x,y,g'')

fromFull :: Square -> Int
fromFull (Full x) = x

shuffled :: IO Board
shuffled = do n  <- randomRIO (300,1000)
              ts <- replicateM n randomIO
              return (shuffle ts startingBoard)
           where
               shuffle []     b = b
               shuffle (t:ts) b = shuffle ts (applyTransformation t b)

removeSquares :: Int -> Board -> IO Board
removeSquares 0 b = return b
removeSquares n b = do [i,j] <- replicateM 2 (randomRIO (0,8))
                       case valueAt i j b of
                           Empty -> removeSquares n b
                           _     -> removeSquares (n - 1) (insertAt i j Empty b)

-- | function to generate a new Sudoku Puzzle with a unique solution
newPuzzle :: IO Board
newPuzzle = do s <- shuffled
               removeMax s

removeMax :: Board -> IO Board
removeMax b = do b' <- removeSquares 1 b
                 if numSolutions b' > 1
                 then return b
                 else removeMax b'

applyTransformation :: Transformation -> Board -> Board
applyTransformation t b = case t of
                              ReflectMajor -> reflectMajor b
                              ReflectMinor -> reflectMinor b
                              Rotate       -> rotate b
                              Swap x y     -> replaceWith x y b

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

replaceWith :: Int -> Int -> Board -> Board
replaceWith x y (Board b) = Board (map (map swap) b)
    where
        swap (Full k) | k == x    = Full y
                      | k == y    = Full x
                      | otherwise = Full k
        swap Empty = Empty

-- | Clockwise Rotate by pi/2
rotate :: Board -> Board
rotate (Board b) = reflectMinor (Board (map reverse b))



-- | functions to specify parts of the board

columns :: Board -> [[Square]]
columns (Board b) = transpose b

rows :: Board -> [[Square]]
rows (Board b) = b

blocks :: Board -> [[Square]]
blocks (Board b) = (concat . map rows2blocks . groupsOfThree) b

sublist :: Int -> Int -> [a] -> [a]
sublist j k xs = take (k - j + 1) (drop j xs)

groupsOfThree :: [a] -> [(a,a,a)]
groupsOfThree (a:b:c:xs) = (a,b,c) : (groupsOfThree xs)
groupsOfThree [] = []

rows2blocks :: ([Square], [Square], [Square]) -> [[Square]]
rows2blocks (a,b,c) = [ concatMap (sublist 0 2) [a,b,c]
                      , concatMap (sublist 3 5) [a,b,c]
                      , concatMap (sublist 6 8) [a,b,c]
                      ]

-- | check whether or not a given puzzle is in a solved state
solved :: Board -> Bool
solved b = all validUnit (rows b ++ columns b ++ blocks b)
    where
        validUnit xs | Empty `elem` xs = False
                     | otherwise       = sort (map fromFull xs) == [1..9]
        fromFull (Full x) = x

valueAt :: Int -> Int -> Board -> Square
valueAt i j (Board b) = b !! i !! j

insertAt :: Int -> Int -> Square -> Board -> Board
insertAt i j s (Board b) = Board (before ++ [newRow] ++ after)
    where
        before = take i b
        newRow = let oldRow = b !! i in take j oldRow ++ [s] ++ drop (j + 1) oldRow
        after  = drop (i + 1) b

incrementAt :: Int -> Int -> Board -> Board
incrementAt i j b = insertAt i j new b
    where
        new = case valueAt i j b of
                  Empty  -> Full 1
                  Full 9 -> Empty
                  Full x -> Full (x + 1)

emptySquares :: Board -> [(Int,Int)]
emptySquares (Board b) = [(i,j) | i <- [0..8], j <- [0..8], b !! i !! j == Empty]

validMove :: Int -> Int -> Int -> Board -> Bool
validMove i j n b =    x `notElem` block
                    && x `notElem` row
                    && x `notElem` col
    where
        block = (blocks b) !! (3 * (i `div` 3) + (j `div` 3))
        row   = rows b !! i
        col   = columns b !! j
        x     = Full n

validState :: Board -> Bool
validState b = (all noDuplicates . map onlyFull) (blocks b ++ rows b ++ columns b)

onlyFull :: [Square] -> [Int]
onlyFull xs = map (\(Full x) -> x) (filter isFull xs)
    where
        isFull (Full _) = True
        isFull _        = False

noDuplicates :: Eq a => [a] -> Bool
noDuplicates xs = xs == nub xs

numSolutions :: Board -> Int
numSolutions b
    | null (emptySquares b) = 1
    | otherwise             = (sum . map numSolutions) newBoards
        where
            newBoards = [insertAt i j (Full x) b | x <- [1..9], validMove i j x b]
            (i,j)     = head (emptySquares b)

toString :: Board -> String
toString (Board b) = (unwords . concatMap (map sqToStr)) b
    where
        sqToStr (Full x) = show x
        sqToStr _        = "0"

getPuzzle :: IO CString
getPuzzle = do p <- removeSquares 1 startingBoard
               newCString (toString p)

foreign export ccall getPuzzle :: IO CString
