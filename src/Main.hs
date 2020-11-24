module Main where
import Solver
import IOHandler

main = do
    board <- readBoard "data/input1.txt"
    let resBoard = solve(board)
    print resBoard
