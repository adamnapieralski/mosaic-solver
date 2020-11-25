module Solver where
import Board
import Neighbour

solve :: Board -> Board
solve inPuzzle = fillNeighbours inPuzzle [C, N, S, W, E, NW, NE, SW, SE] 2 2
    -- fillCell (inPuzzle) (getNbY inPuzzle N 1) (getNbX inPuzzle N 1)
    -- (makeEmptyWithSize (case (getNbY inPuzzle NW 0) of
    --                                     Just y -> y
    --                                     Nothing -> 50) (getW(inPuzzle)))

