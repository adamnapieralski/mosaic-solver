module Board where

import Neighbour
import Utils

data Board = Board [String] deriving (Show)

row :: Board -> Int -> String
row (Board board) y = board !! y

element :: Board -> Int -> Int -> Char
element board y x = (row board y) !! x

makeEmptyWithSize :: Int -> Int -> Board
makeEmptyWithSize y x = Board (replicate y (replicate x ' '))

getH :: Board -> Int
getH (Board board) = length board

getW :: Board -> Int
getW (Board board)  | length board == 0 = 0
                    | otherwise = length (board !! 0)

getNbY :: Board -> Neighbour -> Int -> Maybe Int
getNbY board nb y   | elem nb [NW, N, NE] = (if inRange (y-1) 0 (getH board) then Just (y-1) else Nothing)
                    | elem nb [E, C, W] = Just y
                    | elem nb [SW, S, SE] = (if inRange (y+1) 0 (getH board) then Just (y+1) else Nothing)


getNbX :: Board -> Neighbour -> Int -> Maybe Int
getNbX board nb x   | elem nb [NE, E, SE] = (if inRange (x-1) 0 (getW board) then Just (x-1) else Nothing)
                    | elem nb [N, C, S] = Just x
                    | elem nb [NW, W, SW] = (if inRange (x+1) 0 (getW board) then Just (x+1) else Nothing)

replaceCell :: Board -> Int -> Int -> Char -> Board
replaceCell (Board board) y x newCh = Board (replaceNth y (replaceNth x newCh (row (Board board) y)) board)

fillCell :: Board -> Maybe Int -> Maybe Int -> Board
fillCell markBoard y x =
    case y of
        Just y -> case x of
            Just x -> replaceCell markBoard y x 'X'
            Nothing -> markBoard
        Nothing -> markBoard
