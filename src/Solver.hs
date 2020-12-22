module Solver where
import Board
import Neighbour
import SimpleLogicSolver

import Debug.Trace ( trace )

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

-- | Check if all cells meet specified requirements, that are defines as a functions that takes as the arguments
--   the board, row and columns and returns Bool. 
checkBoard :: Board -> ( Board -> Int -> Int -> Bool) -> Bool 
checkBoard board fun =
    let h = getH board                              
        rows = [0..(h - 1)]
    in iterRow board fun rows True where
    iterRow board _  _ False = False
    iterRow board _  [] res = res 
    iterRow board fun (y:ys) True = iterRow board fun ys (iterCol board fun y [0..(getW board - 1)] True)
    iterCol board _ _ [] res = res
    iterCol board _ _ _ False = False
    iterCol board fun y (x:xs) True = iterCol board fun y xs (iterCell board fun y x True)
    iterCell board _ _ _ False = False
    iterCell board fun y x True = fun board y x 

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

