module Utils
  ( playGame
  , whoWon
  , unusedPositions
  , Move
  , Symbol(..)
  , showBoard
  ) where

import Data.List

-- return the first winning set of cells it finds, or Nothing
result :: Board -> Maybe [Cell]
result board = find full $ rows board ++ cols board ++ diagonals board
  where
    full [a, b, c] = a == b && b == c
    full _ = False
    rows = id
    cols = transpose
    diagonals [[a1, _, b1], [_, c, _], [b2, _, a2]] = [[a1, c, a2], [b1, c, b2]]
    diagonals _ = []

-- Have either of the board filling up or a player winning happened?
finished :: Board -> Bool
finished b
  | null (unusedPositions b) = True
  | result b == Nothing = False
  | otherwise = True

whoWon :: Board -> String
whoWon b =
  case result b of
    Nothing -> "Tied game"
    Just ((Right s):_:_) -> (show s) ++ " player won"
    _ -> error "can't happen!"

replaceNth :: Int -> Symbol -> Board -> Board
replaceNth n s = map (map replace)
  where
    replace (Left n')
      | n' == n = Right s
    replace square = square

unusedPositions :: Board -> [Int]
unusedPositions board = [i | Left i <- concat board]

applyMove :: Int -> Symbol -> Board -> Board
applyMove index symbol board
  | index `notElem` unusedPositions board =
    error (show index ++ showBoard board ++ "\nIllegal move position requested")
  | otherwise = replaceNth index symbol board

playGame :: Move -> Move -> Symbol -> Symbol -> Board
playGame m1 m2 s1 s2 =
  playMidGame m2 m1 s2 s1 (applyMove (m1 s1 b) s1 b)
  where
    b = newBoard

playMidGame :: Move -> Move -> Symbol -> Symbol -> Board -> Board
playMidGame m1 m2 s1 s2 b =
  if finished b
    then b
    else playMidGame m2 m1 s2 s1 (applyMove (m1 s1 b) s1 b)

-- the type of a function that computes a move
type Move = (Symbol -> Board -> Int)

-- a board is a list of lists of either position numbers or played symbols
data Symbol
  = O
  | X
  deriving (Show, Eq)

type Cell = Either Int Symbol

type Board = [[Cell]]

-- return a new board
-- by convention, for noughts and crosses
-- there are three rows and columns
-- a board Cell holds a number until someone moves on it
newBoard :: Board
newBoard = map (map Left) [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

showSquare :: Either Int Symbol -> String
showSquare = either (\n -> " " ++ show n ++ " ") (concat . replicate 3 . show)

showBoard :: Board -> String
showBoard =
  unlines .
  surround "+---+---+---+" . map (concat . surround "|" . map showSquare)
  where
    surround x xs = [x] ++ intersperse x xs ++ [x]
