module Main where
import Solver
import IOHandler
import Board
import Neighbour
import System.Environment

inputFile [x] = x
inputFile _ = "data/input1.txt"

main = do
    args <- getArgs
    board <- readBoard (inputFile args)
    -- let resBoard = solve(board)
    let resBoard = makeEmptyWithSize (getH board) (getW board)
    print(checkNeighboursFill resBoard [N, S, W] 0 2)
    print(getValidNeighbours resBoard 0 0)
    let filledTL = fillFullRemainingOfCell board resBoard 0 0
    putStr (boardToString filledTL)
    putStr "\n"
    putStr (boardToString board)
