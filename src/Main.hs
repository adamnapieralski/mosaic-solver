module Main where
import Solver
import IOHandler ( readBoard )
import Board

main :: IO ()
main = do
    putStrLn ""
    putStrLn "-------------------------------------------------------------------------"
    putStrLn "----------------------- Welcome to mosaic-solver! -----------------------"
    putStrLn "-------------------------------------------------------------------------"
    putStrLn ""
    putStrLn "Please provide a path to the input file with a mosaic you want to solve."
    path <- getLine
    board <- readBoard path
    putStrLn ">>> Input <<<"
    putStr (boardToString board)
    let resBoard = solve board
    putStrLn "<<< Solved >>>"
    putStr (boardToString (makePretty resBoard))

