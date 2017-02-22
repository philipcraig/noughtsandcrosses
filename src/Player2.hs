module Player2
  ( move
  ) where

import Utils

move :: Move
move _s b = last (unusedPositions b)
