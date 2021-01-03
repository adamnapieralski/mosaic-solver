module Main where
import Solver
import IOHandler ( readBoard )
import Board
import Neighbour
import System.Environment ( getArgs )

inputFile :: [[Char]] -> [Char]
inputFile [x] = x
inputFile _ = "data/input4_complex.txt"

main :: IO ()
main = do
    args <- getArgs
    board <- readBoard (inputFile args)
    let resBoard = solve board
    putStr "result \n"
    putStr (boardToString resBoard)
    putStr "pretty \n"
    putStr (boardToString (makePretty resBoard))
    putStr "input \n"
    putStr (boardToString board)
