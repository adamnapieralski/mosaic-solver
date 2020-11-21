module Tree where
data Tree a = Empty | Node a (Tree a) (Tree a)
depth :: Tree a -> Int
depth Empty = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)