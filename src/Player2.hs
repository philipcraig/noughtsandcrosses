module Player2
  ( move
  ) where

import Board

import AI.Eval
import AI.Minimax

move :: MoveF
-- move b = last (unusedPositions b)
move b = m
  where
    (m, _) = searchMove (alphaBeta simpleVal) 4 b
