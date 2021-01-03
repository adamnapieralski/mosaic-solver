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

otherSign :: Char -> Char
otherSign 'X' = '0' 
otherSign '0' = 'X'
otherSign _ = '.'

-- | Change the specified neighbours to the given char. If the neighbour already has
--   a different char it returns Nothing
fillNeighbours :: Board -> [Neighbour] -> Int -> Int -> Char -> Maybe Board
fillNeighbours markBoard [] _ _ _ = Just markBoard
fillNeighbours markBoard (n:nbs) y x sign = do
    nby <- Just (getNbY markBoard n y)
    nbx <- Just (getNbX markBoard n x)
    if maybeToBool (isCellFilled markBoard nby nbx (otherSign sign )) then Nothing
    else fillNeighbours (fillCell markBoard nby nbx sign) nbs y x sign

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

countSignNeighbours :: Board -> Int -> Int -> Char -> Int
countSignNeighbours markBoard y x sign = go markBoard (checkNeighboursFill markBoard [N, NE, E, SE, S, SW, W, NW, C] y x sign) y x 0 where
    go _ [] _ _ counter = counter
    go markBoard (b:bs) y x counter = go markBoard bs y x (counter + addNum) where
        addNum      | b == Just True = 1
                    | otherwise = 0

countNotSignNeighbours :: Board -> Int -> Int -> Char -> Int
countNotSignNeighbours markBoard y x sign = go markBoard (checkNeighboursFill markBoard [N, NE, E, SE, S, SW, W, NW, C] y x sign) y x 0 where
    go _ [] _ _ counter = counter
    go markBoard (b:bs) y x counter = go markBoard bs y x (counter + addNum) where
        addNum      | b == Just False = 1
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

-- fillFullRemainingOfCell :: Board -> Board -> Int -> Int -> Board
-- fillFullRemainingOfCell numBoard markBoard y x
--     | (countNotSignNeighbours markBoard y x 'X') == digitToInt (element numBoard y x) = fillNeighbours markBoard (getValidNeighbours markBoard y x) y x 'X'
--     | otherwise = markBoard

boardToString :: Board -> String
boardToString board = hLine board ++ boardToString_ board ++ hLine board where
    boardToString_ (Board board) = go board
    go [x] = "|" ++ x ++ "|\n"
    go (x:xs) = ("|" ++ x ++ "|\n") ++ go xs
    hLine board = replicate (2 + getW board) '-' ++ "\n"

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

areSame :: Board -> Board -> Bool
areSame board1 board2 = checkDoubleBoard board1 board2 funEq where
    funEq board1 board2 y x = (element board1 y x) == (element board2 y x)
