{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Board
  ( Board
  , Symbol(..)
  , playGame
  , activePlayer
  , whoWon
  , unusedPositions
  , finished
  , applyMove
  , MoveF
  , showBoard
  , winner
  ) where

import Data.List
import Data.Maybe

import AI.Gametree

-- return the first winning set of Cells it finds, or Nothing
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
  | isNothing (result b) = False
  | otherwise = True

-- only defined if called on a game with a winner
winner :: Board -> Symbol
winner b = case result b of
  Just (Right s : (_ : _)) -> s
  _ -> error "can't happen! (did you call on game with no winner?)"

-- only defined if called on a finished game
whoWon :: Board -> String
whoWon b =
  case result b of
    Nothing -> "Tied game"
    Just (Right s : (_ : _)) -> show s ++ " player won"
    _ -> error "can't happen! (did you call on unfinished game?)"

replaceNth :: Int -> Symbol -> Board -> Board
replaceNth n s = map (map replace)
  where
    replace (Left n')
      | n' == n = Right s
    replace square = square

unusedPositions :: Board -> [Int]
unusedPositions board = [i | Left i <- concat board]

applyMove :: Int -> Board -> Board
applyMove index board
  | index `notElem` unusedPositions board =
    error (show index ++ showBoard board ++ "\nIllegal move position requested")
  | otherwise = replaceNth index (activePlayer board) board

playGame :: MoveF -> MoveF -> Board
playGame m1 m2 =
  playMidGame m2 m1 (applyMove (m1 b) b)
  where
    b = newBoard

playMidGame :: MoveF -> MoveF -> Board -> Board
playMidGame m1 m2 b =
  if finished b
    then b
    else playMidGame m2 m1 (applyMove (m1 b) b)

-- the type of a function that computes a move
type MoveF = Board -> Int

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

showSquare :: Cell -> String
showSquare = either (\n -> " " ++ show n ++ " ") (concat . replicate 3 . show)

showBoard :: Board -> String
showBoard =
  unlines .
  surround "+---+---+---+" . map (concat . surround "|" . map showSquare)
  where
    surround x xs = [x] ++ intersperse x xs ++ [x]

-- given a board, which player has the current move?
-- encodes the convention that X goes first.
activePlayer :: Board -> Symbol
activePlayer b =
  if count (Right O) cells < count (Right X) cells then O else X
  where
    cells = concat b
    count x =  length . filter (==x)

-- | instance of the transition system for noughts and crosses
instance Transitions Board Int where
  actions = unusedPositions
  transition = applyMove
