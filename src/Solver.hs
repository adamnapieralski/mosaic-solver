module Solver where
import Board

solve :: Board -> Board
solve inPuzzle = makeEmptyWithSize (getH(inPuzzle)) (getW(inPuzzle))
