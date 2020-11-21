module Board where

data Board = Board [String] deriving (Show)

row :: Board -> Int -> String
row (Board board) y = board !! y

element :: Board -> Int -> Int -> Char
element board y x = (row board y) !! x

makeEmptyWithSize :: Int -> Int -> Board
makeEmptyWithSize x y = Board (replicate y (replicate x ' '))

getH :: Board -> Int
getH (Board board) = length board

getW :: Board -> Int
getW (Board board)  | length board == 0 = 0
                    | otherwise = length (board !! 0)