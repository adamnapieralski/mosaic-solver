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
-- | Get row of the specified cell's neighbour.
getNbY :: Board -> Neighbour -> Int -> Maybe Int
getNbY board nb y   | elem nb [NW, N, NE] = (if inRange (y-1) 0 (getH board) then Just (y-1) else Nothing)
                    | elem nb [E, C, W] = Just y
                    | elem nb [SW, S, SE] = (if inRange (y+1) 0 (getH board) then Just (y+1) else Nothing)

-- | Get column of the specified cell's neighbour.
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

-- |Check if the given neighbour has the specified char. 
isNeighbourFilled :: Board -> Neighbour -> Int -> Int -> Char -> Bool
isNeighbourFilled board nb y x sign =
    case isCellFilled board (getNbY board nb y) (getNbX board nb x) sign of
        Just a -> a
        Nothing -> False

fillCell :: Board -> Maybe Int -> Maybe Int -> Char -> Board
fillCell markBoard y x sign =
    case y of
        Just y -> case x of
            Just x -> replaceCell markBoard y x sign
            Nothing -> markBoard
        Nothing -> markBoard

-- | Change the specified neighbours to the given char.
fillNeighbours :: Board -> [Neighbour] -> Int -> Int -> Char -> Board
fillNeighbours markBoard [] _ _ _ = markBoard
fillNeighbours markBoard (n:nbs) y x sign =
    fillNeighbours (fillCell markBoard (getNbY markBoard n y) (getNbX markBoard n x) sign)
        nbs y x sign

-- | Check if specified cell has the given char.
isCellFilled :: Board -> Maybe Int -> Maybe Int -> Char -> Maybe Bool
isCellFilled _ Nothing _ _ = Nothing
isCellFilled _ _ Nothing _ = Nothing
isCellFilled markBoard y x sign =
    case y of
        Just y -> case x of
            Just x -> Just (element markBoard y x == sign)
            Nothing -> Nothing
        Nothing -> Nothing

checkNeighboursFill :: Board -> [Neighbour] -> Int -> Int -> Char -> [Maybe Bool]
checkNeighboursFill markBoard nbs y x sign = go markBoard nbs [] y x sign where
    go _ [] fills _ _ _ = fills
    go markBoard (n:nbs) fills y x sign = go markBoard nbs (fills ++ [(isCellFilled markBoard (getNbY markBoard n y) (getNbX markBoard n x) sign)]) y x sign

countBlankNeighbours :: Board -> Int -> Int -> Int
countBlankNeighbours markBoard y x = go markBoard (checkNeighboursFill markBoard [N, NE, E, SE, S, SW, W, NW, C] y x 'X') y x 0 where
    go _ [] _ _ counter = counter
    go markBoard (b:bs) y x counter = go markBoard bs y x (counter + blankAdd) where
        blankAdd    | b == Just False = 1
                    | otherwise = 0

getValidNeighbours :: Board -> Int -> Int -> [Neighbour]
getValidNeighbours board y x = go board [C] [N, NE, E, SE, S, SW, W, NW] y x where
    go _ validNeighbours [] _ _ = validNeighbours
    go board validNeighbours (n:nbs) y x    | isNeighbourValid board n y x = go board (n:validNeighbours) nbs y x
                                            | otherwise = go board validNeighbours nbs y x

getFilledNeighbours :: Board -> Int -> Int -> Char -> [Neighbour]
getFilledNeighbours board y x sign = go board [] (getValidNeighbours board y x) y x sign where
    go _ filledNeighbours [] _ _ _ = filledNeighbours
    go board filledNeighbours (n:nbs) y x sign | isNeighbourFilled board n y x sign = go board (n:filledNeighbours) nbs y x sign
                                               | otherwise = go board filledNeighbours nbs y x sign

fillFullRemainingOfCell :: Board -> Board -> Int -> Int -> Board
fillFullRemainingOfCell numBoard markBoard y x
    | (countBlankNeighbours markBoard y x) == digitToInt (element numBoard y x) = fillNeighbours markBoard (getValidNeighbours markBoard y x) y x 'X'
    | otherwise = markBoard

boardToString :: Board -> String
boardToString board = hLine board ++ boardToString_ board ++ hLine board where
    boardToString_ (Board board) = go board
    go [x] = "|" ++ x ++ "|\n"
    go (x:xs) = ("|" ++ x ++ "|\n") ++ go xs
    hLine board = replicate (2 + getW board) '-' ++ "\n"