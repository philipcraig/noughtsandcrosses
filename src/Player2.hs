module Player2
  ( move
  ) where

import Board

import AI.Minimax
import AI.Eval

move :: MoveF
-- move b = last (unusedPositions b)
move b = m
  where
  (m, _) = searchMove (alphaBeta simpleVal) 4 b