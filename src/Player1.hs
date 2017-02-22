module Player1
  ( move
  ) where

import Utils

move :: Move
move _s b = head (unusedPositions b)
