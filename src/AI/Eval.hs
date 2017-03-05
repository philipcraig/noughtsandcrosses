--
-- Static evaluation functions for board positions
--
module AI.Eval
  ( simpleVal
  ) where

import AI.Minimax
import Board

-- simple static board valuation (material score only)
simpleVal :: Board -> Value
simpleVal b =
  if finished b
    then (-maxBound)
    else 1
