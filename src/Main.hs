module Main where

import Player1
import Player2
import Utils

main :: IO ()
main =
    putStrLn message 
    where
        message = displayBoard res ++ "\n" ++ whoWon res 'X' 'O'
        res = playgame Player1.move Player2.move 'X' 'O' getstart