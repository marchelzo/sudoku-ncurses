import SudokuCore

import Control.Applicative (liftA)
import Text.Parsec
import Text.Parsec.String
import Data.List.Split (chunksOf)

parsePuzzle :: Parser Board
parsePuzzle = do ns <- liftA (map (toSq . read . return)) (sepBy digit (char ' '))
                 return (Board (chunksOf 9 ns))
              where
                  toSq 0 = Empty
                  toSq x = Full x
                 
