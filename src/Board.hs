module Board where

import Neighbour
import Utils

import Data.Char

-- | Board represents mosaic's board used in solving process
--   both for input (numeric) and output (Char/sign marked)
newtype Board = Board [String] deriving (Show)

-- | Get specific row of board
row :: Board -> Int -> String
row (Board board) y = board !! y

-- | Get specific element of board at position (y, x) = (row, col)
element :: Board -> Int -> Int -> Char
element board y x = row board y !! x

-- | Return embty board of size (y, x)
makeEmptyWithSize :: Int -> Int -> Board
makeEmptyWithSize y x = Board (replicate y (replicate x ' '))

-- | Get height of board
getH :: Board -> Int
getH (Board board) = length board

-- | Get width of board
getW :: Board -> Int
getW (Board board)  | null board = 0
                    | otherwise = length (head board)

-- | Get row of the specified cell's neighbour
getNbY :: Board -> Neighbour -> Int -> Maybe Int
getNbY board nb y   | nb `elem` [NW, N, NE] = if inRange (y-1) 0 (getH board) then Just (y-1) else Nothing
                    | nb `elem` [E, C, W] = Just y
                    | nb `elem` [SW, S, SE] = if inRange (y+1) 0 (getH board) then Just (y+1) else Nothing

-- | Get column of the specified cell's neighbour
getNbX :: Board -> Neighbour -> Int -> Maybe Int
getNbX board nb x   | nb `elem` [NE, E, SE] = if inRange (x+1) 0 (getW board) then Just (x+1) else Nothing
                    | nb `elem` [N, C, S] = Just x
                    | nb `elem` [NW, W, SW] = if inRange (x-1) 0 (getW board) then Just (x-1) else Nothing

-- | Replace cell at specific position with Char and return modified board
replaceCell :: Board -> Int -> Int -> Char -> Board
replaceCell (Board board) y x newCh = Board (replaceNth y (replaceNth x newCh (row (Board board) y)) board)

-- | Check if element's neighbour exists - doesn't exceed board's size
isNeighbourValid :: Board -> Neighbour -> Int -> Int -> Bool
isNeighbourValid board n y x =
    case getNbY board n y of
        Just y -> case getNbX board n x of
            Just x -> True
            Nothing -> False
        Nothing -> False

-- | Check if the given neighbour has the specified Char
isNeighbourFilled :: Board -> Neighbour -> Int -> Int -> Char -> Bool
isNeighbourFilled board nb y x sign =
    case isCellFilled board (getNbY board nb y) (getNbX board nb x) sign of
        Just a -> a
        Nothing -> False

-- | Fill valid cell with Char
fillCell :: Board -> Maybe Int -> Maybe Int -> Char -> Board
fillCell markBoard y x sign =
    case y of
        Just y -> case x of
            Just x -> replaceCell markBoard y x sign
            Nothing -> markBoard
        Nothing -> markBoard

-- | Get 'opposite' sign used in result board marking
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

-- | Get list of valid (not exceeding board's size) neighbours
getValidNeighbours :: Board -> Int -> Int -> [Neighbour]
getValidNeighbours board y x = go board [C] [N, NE, E, SE, S, SW, W, NW] y x where
    go _ validNeighbours [] _ _ = validNeighbours
    go board validNeighbours (n:nbs) y x | isNeighbourValid board n y x = go board (n:validNeighbours) nbs y x
                                         | otherwise = go board validNeighbours nbs y x

-- | Get list of neighbours filled with specific Char sign
getFilledNeighbours :: Board -> Int -> Int -> Char -> [Neighbour]
getFilledNeighbours board y x sign = go board [] (getValidNeighbours board y x) y x sign where
    go _ filledNeighbours [] _ _ _ = filledNeighbours
    go board filledNeighbours (n:nbs) y x sign | isNeighbourFilled board n y x sign = go board (n:filledNeighbours) nbs y x sign
                                               | otherwise = go board filledNeighbours nbs y x sign

-- | Parse a board to a multiline string - for printing in console
boardToString :: Board -> String
boardToString board = hLine board ++ boardToString_ board ++ hLine board where
    boardToString_ (Board board) = go board
    go [x] = "|" ++ x ++ "|\n"
    go (x:xs) = ("|" ++ x ++ "|\n") ++ go xs
    hLine board = replicate (2 + getW board) '-' ++ "\n"

-- | Modify the board to have only 'X' and empty cells - for prettier printing in console
makePretty :: Board -> Board
makePretty board = go board 0 0 where
    go board y x | x >= getW board = go board (y+1) 0
                 | y >= getH board = board
                 | isNeighbourFilled board C y x '0' = go (replaceCell board y x ' ') y (x+1)
                 | otherwise = go board y (x+1)

-- | Check if all cells meet specified requirement, that is defined as a function that takes as the arguments
--   two boards, row and column and returns Bool. 
checkDoubleBoard :: Board -> Board -> ( Board -> Board -> Int -> Int -> Bool) -> Bool
checkDoubleBoard board1 board2 fun = go board1 board2 fun True 0 0 where
    go board1 board2 fun check y x | x >= getW board1 = go board1 board2 fun check (y+1) 0 
                                   | y >= getH board1 = check
                                   | not check = False
                                   | otherwise = go board1 board2 fun (fun board1 board2 y x) y (x+1)

-- | Check if all cells meet specified requirements, that are defined as a function that takes as the arguments
--   the board, row and column and returns Bool. 
checkBoard :: Board -> ( Board -> Int -> Int -> Bool) -> Bool 
checkBoard board fun = checkDoubleBoard board board funDouble where
    funDouble board _ y x = fun board y x

-- | Check if two boards are the same - all their cells have the same values
areSame :: Board -> Board -> Bool
areSame board1 board2 = checkDoubleBoard board1 board2 funEq where
    funEq board1 board2 y x = element board1 y x == element board2 y x
