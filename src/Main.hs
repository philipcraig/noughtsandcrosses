module Main where

import Player1
import Player2
import Utils

main :: IO ()
main = putStrLn message
  where
    message = (showBoard res) ++ "\n" ++ whoWon res X O
    res = playGame Player1.move Player2.move X O newBoard
