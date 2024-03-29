-- EPITECH PROJECT, 2023
-- B-FUN-500-PAR-5-2-glados-florian.labarre
-- File description:
-- Position.hs

module Packrat.Position (Pos(..), nextPos) where

data Pos = Pos {
    file :: String,
    line :: Int,
    col :: Int,
    tab_len :: Int
} deriving (Eq)

nextPos :: Pos -> Char -> Pos
nextPos p@Pos{line = l} '\n' = p{line = l + 1, col = 1}
nextPos p@Pos{col = c, tab_len = l} '\t' = p{col = c + l}
nextPos p@Pos{col = c} _ = p{col = c + 1}

instance Show Pos where
    show (Pos f l c _) = show f ++ ":" ++ show l ++ ":" ++ show c ++ ": "

instance Ord Pos where
    Pos _ l1 c1 _ <= Pos _ l2 c2 _ = (l1 < l2) || (l1 == l2 && c1 <= c2)