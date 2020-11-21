module IOHandler where
import Board

import System.IO (readFile)
-- read puzzle from the given source file name
readPuzzle :: String -> IO [String]
readPuzzle filename = do
    contents <- readFile filename
    -- read whole file contents
    let puzzle = read contents :: [String] -- create list of strings (see Read type class)
    return puzzle

readBoard :: String -> IO Board
readBoard filename = do
    puzzle <- readPuzzle filename
    return (Board puzzle)
