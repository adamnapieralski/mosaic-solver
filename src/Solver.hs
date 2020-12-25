module Solver where
import Board
import Neighbour
import SimpleLogicSolver

import Debug.Trace ( trace )
import Data.Char

-- | Solve the mosaic problem
solve :: Board -> Board
solve numBoard = let h = getH numBoard
                     w = getW numBoard
                     resBoard = makeEmptyWithSize h w
                 in solveSimpleLogic numBoard resBoard

-- | Modify the board to have only 'X' and empty cells
makePretty :: Board -> Board 
makePretty board = iterateBoard board board makeCellPretty where
    makeCellPretty _ board y x | isNeighbourFilled board C y x '0' = replaceCell board y x ' '
                               | otherwise = board


-- | Apply the specified function to all cells in the board. The function arguments are the following:
--   the input board, the output board with, row, column. The function must return updated output board. 
iterateBoard :: Board -> Board -> ( Board -> Board -> Int -> Int -> Board) -> Board 
iterateBoard numBoard resBoard fun =
    let h = getH numBoard                              
        rows = [0..(h - 1)]
    in iterRow numBoard resBoard fun rows where
    iterRow numBoard resBoard _ [] = resBoard
    iterRow numBoard resBoard fun (y:ys) = iterRow numBoard (iterCol numBoard resBoard fun y [0..(getW numBoard - 1)]) fun ys
    iterCol numBoard resBoard _ _ [] = resBoard
    iterCol numBoard resBoard fun y (x:xs) = iterCol numBoard (iterCell numBoard resBoard fun y x) fun y xs
    iterCell numBoard resBoard fun y x = fun numBoard resBoard y x 

-- | Check if all cells meet specified requirement, that are defined as a function that takes as the arguments
--   two boards, row and column and returns Bool. 
checkDoubleBoard :: Board -> Board -> ( Board -> Board -> Int -> Int -> Bool) -> Bool
checkDoubleBoard board1 board2 fun =
    let h = getH board1
        rows = [0..(h - 1)]
    in iterRow board1 board2 fun rows True where
    iterRow _ _ _ _ False = False
    iterRow _ _ _ [] res = res
    iterRow board1 board2 fun (y:ys) True = iterRow board1 board2 fun ys (iterCol board1 board2 fun y [0..(getW board1 - 1)] True)
    iterCol _ _ _ _ _ False = False
    iterCol _ _ _ _ [] res = res
    iterCol board1 board2 fun y (x:xs) True = iterCol board1 board2 fun y xs (iterCell board1 board2 fun y x True)
    iterCell _ _ _ _ _ False = False
    iterCell board1 board2 fun y x True = fun board1 board2 y x

-- | Check if all cells meet specified requirements, that are defined as a function that takes as the arguments
--   the board, row and column and returns Bool. 
checkBoard :: Board -> ( Board -> Int -> Int -> Bool) -> Bool 
checkBoard board fun = checkDoubleBoard board board funDouble where
    funDouble board _ y x = fun board y x

-- | Solve the mosaic problem using basic logic. Cells are updated only when they can be clearly marked as filled or empty.
--   The alorithm iterates over the board until all cells are defined or iterations limit is reached.
solveSimpleLogic :: Board -> Board -> Board
solveSimpleLogic numBoard resBoard = go numBoard resBoard 10 False where    
    go _ resBoard _ _ | trace (boardToString resBoard) False = undefined
    go _ resBoard _ True = resBoard
    go _ resBoard 0 _ = resBoard    
    go numBoard resBoard x solved = go numBoard (iterateBoard numBoard resBoard SimpleLogicSolver.processCell) (x-1) (checkIfSolved resBoard)

-- | Check if all cells are defined as filled or empty.
checkIfSolved:: Board -> Bool 
checkIfSolved board = checkBoard board check where
    check board y x = isNeighbourFilled board C y x '0' || isNeighbourFilled board C y x 'X'

-- | Check if all marked solutions in resBoard are valid
checkIfValid:: Board -> Board -> Bool
checkIfValid resBoard numBoard = checkDoubleBoard resBoard numBoard check where
    check resBoard numBoard y x =
        if element numBoard y x == '.' then True
        else countSignNeighbours resBoard y x 'X' <= digitToInt (element numBoard y x) &&
            countSignNeighbours resBoard y x '0' <= 9 - digitToInt (element numBoard y x)
