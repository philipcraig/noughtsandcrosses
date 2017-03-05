module Main where

import Board
import Player1
import Player2

main :: IO ()
main = do
  putStrLn $ showBoard result
  putStrLn $ whoWon result
  where
    result = playGame Player1.move Player2.move
