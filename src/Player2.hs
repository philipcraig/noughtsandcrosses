{-# LANGUAGE RankNTypes #-}

module Player2
( move
) where

indexOfFirstBlank :: Num t => t -> String -> t
indexOfFirstBlank n b
    | head b == '-' = n
    | otherwise = indexOfFirstBlank (n + 1) (tail b)

move :: Char -> String -> Int
move _s = indexOfFirstBlank 0