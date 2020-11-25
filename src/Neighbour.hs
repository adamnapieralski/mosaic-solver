module Neighbour where

data Neighbour = N | NE | E | SE | S | SW | W | NW | C deriving Show

instance Eq Neighbour where
    N == N = True
    NE == NE = True
    E == E = True
    SE == SE = True
    S == S = True
    SW == SW = True
    W == W = True
    NW == NW = True
    _ == _ = False