module Utils where

inRange :: Int -> Int -> Int -> Bool
inRange val min max | val >= min && val < max = True
                    | otherwise = False

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

maybeToBool :: Maybe Bool -> Bool
maybeToBool Nothing = False
maybeToBool (Just a) = a

-- | Get 'opposite' sign used in result board marking
otherSign :: Char -> Char
otherSign 'X' = '0'
otherSign '0' = 'X'
otherSign _ = '.'

fromJust :: Maybe a -> a
fromJust (Just x) = x

digitToInt :: Char -> Int
digitToInt c = fromEnum c - fromEnum '0'

intToDigit :: Int -> Char
intToDigit i = toEnum (i+48)::Char

-- | Difference of sets x and y represented as lists
(\\) :: (Foldable t, Eq a) => [a] -> t a -> [a]
(\\) x y = go x y [] where
    go [] _ res = res
    go (x:xs) y res = if (x `elem` y) then go xs y res else go xs y (x:res)