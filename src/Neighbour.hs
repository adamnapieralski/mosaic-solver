module Neighbour where

-- | Neighbour represents cells in relative position to the given cell
--   including the cell itself
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
    C == C = True
    _ == _ = False