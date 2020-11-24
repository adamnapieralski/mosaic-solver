module Solver where
import Board
import Neighbour

solve :: Board -> Board
solve inPuzzle = fillCell (inPuzzle) (getNbY inPuzzle N 1) (getNbX inPuzzle N 1)
    -- (makeEmptyWithSize (case (getNbY inPuzzle NW 0) of
    --                                     Just y -> y
    --                                     Nothing -> 50) (getW(inPuzzle)))

