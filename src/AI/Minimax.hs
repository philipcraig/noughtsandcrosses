{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}

module AI.Minimax
   ( 
    Value
   , searchMove
   , alphaBeta
   ) where

import AI.Gametree

-- | a type for game position valuations;
-- simply a wrapper newtype over integers
newtype Value = Value Int deriving (Eq, Ord, Enum, Bounded,
                                    Num, Real, Integral, Show, Read)

-- compute best move using some search function
-- undefined for terminal positions
searchMove :: Transitions s' l =>
              (Value -> Value -> Int -> s' -> Value)
              -> Int -> s' -> (l, Value)
searchMove abSearch depth s' = cmx (minBound+1) first (transitions s')
  where
    first = head (actions s')
    cmx !alpha best [] = (best, alpha)
    cmx !alpha best ((l,s):rest) = cmx alpha' best' rest
        where !v = - abSearch (-maxBound) (-alpha) (depth-1) s
              !alpha' = if v>alpha then v else alpha
              !best' = if v>alpha then l else best

-- compute minimax value using Negamax with alpha-beta prunning
alphaBeta :: Transitions s l =>
             (s -> Value) -> Value -> Value -> Int -> s -> Value
alphaBeta valf alpha' beta depth s
  | depth==0 || isTerminal s = valf s
  | otherwise = cmx alpha' (successors s)
  where
    cmx !alpha [] = alpha
    cmx !alpha (p:ps)
      | a'>=beta = a'
      | otherwise = cmx (max a' alpha) ps
        where a' = - alphaBeta valf (-beta) (-alpha) (depth-1) p