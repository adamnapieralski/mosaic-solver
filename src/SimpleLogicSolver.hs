module SimpleLogicSolver where
import Board
import Data.Char ( digitToInt )
import Data.List ( (\\) )
import Data.Maybe

-- |For the given cell update its neighbours using basic logic,
--  i.e. modify them only if they can be clearly defined
processNeighbourCells :: Board -> Board -> Int -> Int -> Maybe Board
processNeighbourCells numBoard resBoard y x =
    let ns = getValidNeighbours numBoard y x
        nsX = getFilledNeighbours resBoard y x 'X'
        ns0 = getFilledNeighbours resBoard y x '0'
        cellNum = digitToInt (element numBoard y x)
    in  go ns nsX ns0 cellNum where
                              -- contradiction
        go ns nsX ns0 cellNum | length nsX > cellNum || length ns0 > (length ns - cellNum) = Nothing
                              -- number in the cell equals the number of the neighbours.
                              | length ns == cellNum = fillNeighbours resBoard ns y x 'X'
                              -- number in the cell equals the number of the filled neighbours, remaining cells must be empty.
                              | length nsX == cellNum = fillNeighbours resBoard (ns \\ nsX) y x '0'
                              -- number of cells that are clearly empty equals the difference between all neighbours and
                              -- the number in the cell, remaining cells must be filled.
                              | length ns - length ns0 == cellNum = fillNeighbours resBoard (ns \\ ns0) y x 'X'
                              | otherwise = Just resBoard

-- | Iterate the board and try to solve it using simple logic. If Nothing is returned it means tha the board can't be solved.
processBoard :: Board -> Board -> Int -> Int -> Maybe Board
processBoard numBoard resBoard y x = go numBoard (Just resBoard) y x where
    go _ Nothing  _ _ = Nothing
    go numBoard resBoard y x | x >= getW (fromJust resBoard) = go numBoard resBoard (y+1) 0 
                             | y >= getH (fromJust resBoard) = resBoard
                             | element numBoard y x == '.' = go numBoard resBoard y (x+1)
                             | otherwise = go numBoard (processNeighbourCells numBoard (fromJust resBoard) y x) y (x+1)
