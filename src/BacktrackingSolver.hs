module BacktrackingSolver where
import Board

import SimpleLogicSolver
import Neighbour
import Utils ( fromJust ) 

checkSolved :: Board -> Bool
checkSolved board = checkBoard board check where
    check board y x = isNeighbourFilled board C y x '0' || isNeighbourFilled board C y x 'X'

-- | Iterate the board and try to solve it using backtracking. If Nothing is returned it means that the board can't be solved.
processBoard :: Board -> Board -> Maybe Board
processBoard numBoard resBoard = go numBoard (Just resBoard) 0 0 where
    go _ Nothing  _ _ = Nothing
    go numBoard resBoard y x | x >= getW (fromJust resBoard) = go numBoard resBoard (y+1) 0 
                             | y >= getH (fromJust resBoard) = resBoard
                             | element (fromJust resBoard) y x == 'X' || element (fromJust resBoard) y x == '0' = go numBoard resBoard y (x+1)
                             --- try to solve using logic, it needs to start analysis earlier to ensure it solves it properly
                             | otherwise = case SimpleLogicSolver.processBoard numBoard (fromJust resBoard) (max (y-2) 0) (max (x-2) 0) of
                                                -- if simple logic solved it, the solved board is returned
                                                Just board -> if checkSolved board then Just board
                                                              -- if simple logic solver didn't make any changes, the current cell is filled with 'X' and the solution
                                                              -- continues recursively. If the guess was wrong, the cell is filled with '0'
                                                              else if areSame board (fromJust resBoard) then
                                                                  case go numBoard (Just (fillCell (fromJust resBoard) (Just y) (Just x) 'X')) y (x+1) of
                                                                    Just board -> Just board
                                                                    Nothing -> go numBoard (Just (fillCell (fromJust resBoard) (Just y) (Just x) '0')) y (x+1)
                                                              -- if simple logic made some progress it is run again
                                                              else go numBoard (Just board) y x
                                                -- if simple logic returns Nothing, there is a contradiction so the current solution is abandoned 
                                                Nothing -> Nothing 