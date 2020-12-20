module SimpleLogicSolver where
import Board
import Data.Char ( digitToInt )
-- import Debug.Trace ( trace )
import Data.List ( (\\) )

processCell :: Board -> Board -> Int -> Int -> Board
processCell numBoard resBoard y x
    | element numBoard y x == '.' = resBoard
    | otherwise = processNeighbourCells numBoard resBoard y x


processNeighbourCells :: Board -> Board -> Int -> Int -> Board
processNeighbourCells numBoard resBoard y x =
    let ns = getValidNeighbours numBoard y x
        nsX = getFilledNeighbours resBoard y x 'X'
        ns0 = getFilledNeighbours resBoard y x '0'
        cellNum = digitToInt (element numBoard y x)
    in go ns nsX ns0 cellNum where
        -- go ns nsX ns0 cellNum | trace ("processNeighbourCells (" ++ show y ++ " " ++ show x ++ ") " ++ show ns ++ " " ++ show nsX ++ " " ++ show cellNum) False = undefined
        go ns nsX ns0 cellNum | length ns == cellNum = fillNeighbours resBoard ns y x 'X'
                              | length nsX == cellNum = fillNeighbours resBoard (ns \\ nsX) y x '0'
                              | length ns - length ns0 == cellNum = fillNeighbours resBoard (ns \\ ns0) y x 'X'
                              | otherwise = resBoard

