module Player2
(move
) where
import Debug.Trace

indexOfFirstBlank n b
  | (head b) == '-' = n
  | otherwise = (indexOfFirstBlank (n+1) (tail b))

move s b = (indexOfFirstBlank 0 b)

