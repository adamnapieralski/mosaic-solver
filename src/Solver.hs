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
                 in solveSimpleLogic numBoard resBoard numBoard

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

-- | Solve the mosaic problem using basic logic. Cells are updated only when they can be clearly marked as filled or empty.
--   The alorithm iterates over the board until all cells are defined or no more changes can be made in simple logic way
solveSimpleLogic :: Board -> Board -> Board -> Board
solveSimpleLogic numBoard resBoard prevBoard = go numBoard resBoard prevBoard False where
    go _ resBoard _ _ | trace (boardToString resBoard) False = undefined
    go _ resBoard _ True = resBoard
    go numBoard resBoard prevBoard solved = go numBoard (SimpleLogicSolver.processBoard numBoard resBoard 0 0) resBoard (simpleLogicStopValidation resBoard prevBoard)


simpleLogicStopValidation :: Board -> Board -> Bool
simpleLogicStopValidation resBoard prevBoard = checkIfSolved resBoard || areSame resBoard prevBoard

-- | Check if all cells are defined as filled or empty.
checkIfSolved :: Board -> Bool
checkIfSolved board = checkBoard board check where
    check board y x = isNeighbourFilled board C y x '0' || isNeighbourFilled board C y x 'X'

-- | Check if all marked solutions in resBoard are valid
checkIfValid :: Board -> Board -> Bool
checkIfValid resBoard numBoard = checkDoubleBoard resBoard numBoard check where
    check resBoard numBoard y x =
        if element numBoard y x == '.' then True
        else countSignNeighbours resBoard y x 'X' <= digitToInt (element numBoard y x) &&
            countSignNeighbours resBoard y x '0' <= 9 - digitToInt (element numBoard y x)
