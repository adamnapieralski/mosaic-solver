module Board where

import Neighbour
import Utils

-- used this import, dont know if it isnt against task, but cant think of other way
import Data.Char

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
getNbX board nb x   | elem nb [NE, E, SE] = (if inRange (x+1) 0 (getW board) then Just (x+1) else Nothing)
                    | elem nb [N, C, S] = Just x
                    | elem nb [NW, W, SW] = (if inRange (x-1) 0 (getW board) then Just (x-1) else Nothing)

replaceCell :: Board -> Int -> Int -> Char -> Board
replaceCell (Board board) y x newCh = Board (replaceNth y (replaceNth x newCh (row (Board board) y)) board)

isNeighbourValid :: Board -> Neighbour -> Int -> Int -> Bool
isNeighbourValid board n y x =
    case getNbY board n y of
        Just y -> case getNbX board n x of
            Just x -> True
            Nothing -> False
        Nothing -> False


fillCell :: Board -> Maybe Int -> Maybe Int -> Board
fillCell markBoard y x =
    case y of
        Just y -> case x of
            Just x -> replaceCell markBoard y x 'X'
            Nothing -> markBoard
        Nothing -> markBoard

fillNeighbours :: Board -> [Neighbour] -> Int -> Int -> Board
fillNeighbours markBoard [] _ _ = markBoard
fillNeighbours markBoard (n:nbs) y x =
    fillNeighbours (fillCell markBoard (getNbY markBoard n y) (getNbX markBoard n x))
        nbs y x

isCellFilled :: Board -> Maybe Int -> Maybe Int -> Maybe Bool
isCellFilled _ Nothing _ = Nothing
isCellFilled _ _ Nothing = Nothing
isCellFilled markBoard y x = 
    case y of
        Just y -> case x of
            Just x -> Just (element markBoard y x == 'X')
            Nothing -> Nothing
        Nothing -> Nothing

checkNeighboursFill :: Board -> [Neighbour] -> Int -> Int -> [Maybe Bool]
checkNeighboursFill markBoard nbs y x = go markBoard nbs [] y x where
    go _ [] fills _ _ = fills
    go markBoard (n:nbs) fills y x = go markBoard nbs (fills ++ [(isCellFilled markBoard (getNbY markBoard n y) (getNbX markBoard n x))]) y x

countBlankNeighbours :: Board -> Int -> Int -> Int
countBlankNeighbours markBoard y x = go markBoard (checkNeighboursFill markBoard [N, NE, E, SE, S, SW, W, NW, C] y x) y x 0 where
    go _ [] _ _ counter = counter
    go markBoard (b:bs) y x counter = go markBoard bs y x (counter + blankAdd) where
        blankAdd    | b == (Just False) = 1
                    | otherwise = 0

getValidNeighbours :: Board -> Int -> Int -> [Neighbour]
getValidNeighbours board y x = go board [C] [N, NE, E, SE, S, SW, W, SW] y x where
    go _ validNeighbours [] _ _ = validNeighbours
    go board validNeighbours (n:nbs) y x    | isNeighbourValid board n y x = go board (n:validNeighbours) nbs y x
                                            | otherwise = go board validNeighbours nbs y x


fillFullRemainingOfCell :: Board -> Board -> Int -> Int -> Board
fillFullRemainingOfCell numBoard markBoard y x
    | (countBlankNeighbours markBoard y x) == digitToInt (element numBoard y x) = fillNeighbours markBoard (getValidNeighbours markBoard y x) y x
    | otherwise = markBoard
