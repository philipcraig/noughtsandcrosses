{-# LANGUAGE RankNTypes #-}

module Player1
( move
)
where

indexOfFirstBlank :: forall t. Num t => t -> String -> t
indexOfFirstBlank n b
    | head b == '-' = n
    | otherwise = indexOfFirstBlank (n + 1) (tail b)

move :: forall t t1. Num t1 => t -> String -> t1
move s = indexOfFirstBlank 0