module Player1
( move
)
where

import Types

indexOfFirstBlank :: Int -> String -> Int
indexOfFirstBlank n b
    | head b == '-' = n
    | otherwise = indexOfFirstBlank (n + 1) (tail b)

move :: Move
move _s = indexOfFirstBlank 0