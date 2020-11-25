module Utils where

inRange :: Int -> Int -> Int -> Bool
inRange val min max | val >= min && val < max = True
                    | otherwise = False

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs
