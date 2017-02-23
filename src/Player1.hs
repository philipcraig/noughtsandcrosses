module Player1
  ( move
  ) where

import Board

move :: MoveF
-- move b = head (unusedPositions b)
move b = last (unusedPositions b)
