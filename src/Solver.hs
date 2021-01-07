module Solver where
import Board
import Neighbour
import SimpleLogicSolver
import BacktrackingSolver

import Data.Char
import Data.Maybe

-- | Solve the mosaic problem
solve :: Board -> Board
solve numBoard = let h = getH numBoard
                     w = getW numBoard
                     resBoard = makeEmptyWithSize h w in
                     go (solveSimpleLogic numBoard resBoard numBoard) where
                     go resBoard | checkIfSolved resBoard = resBoard
                                 | otherwise = solveBacktracking numBoard resBoard numBoard

-- | Solve the mosaic problem using basic logic. Cells are updated only when they can be clearly marked as filled or empty.
--   The alorithm iterates over the board until all cells are defined or no more changes can be made in simple logic way
solveSimpleLogic :: Board -> Board -> Board -> Board
solveSimpleLogic numBoard resBoard prevBoard = go numBoard (Just resBoard) (Just prevBoard) False where
    go _ resBoard _ True = fromJust resBoard
    go numBoard Nothing _ _ = makeEmptyWithSize (getH numBoard) (getW numBoard)
    go numBoard resBoard prevBoard solved = go numBoard (SimpleLogicSolver.processBoard numBoard (fromJust resBoard) 0 0) resBoard (simpleLogicStopValidation (fromJust resBoard) (fromJust prevBoard))

-- | Solve the mosaic problem using advanced logic - backtracking.
solveBacktracking :: Board -> Board -> Board -> Board
solveBacktracking numBoard resBoard prevBoard = go numBoard (Just resBoard) (Just prevBoard) False where
    go _ resBoard _ True = fromJust resBoard
    go numBoard Nothing _ _ = makeEmptyWithSize (getH numBoard) (getW numBoard)
    go numBoard resBoard prevBoard solved = go numBoard (BacktrackingSolver.processBoard numBoard (fromJust resBoard)) resBoard (simpleLogicStopValidation (fromJust resBoard) (fromJust prevBoard))


-- | check if simple logic solving should stop:
--   board is solved (all cells are filled) or no changes were made to the board
simpleLogicStopValidation :: Board -> Board -> Bool
simpleLogicStopValidation resBoard prevBoard = checkIfSolved resBoard || areSame resBoard prevBoard

-- | Check if all cells are defined as filled or empty.
checkIfSolved :: Board -> Bool
checkIfSolved board = checkBoard board check where
    check board y x = isNeighbourFilled board C y x '0' || isNeighbourFilled board C y x 'X'
