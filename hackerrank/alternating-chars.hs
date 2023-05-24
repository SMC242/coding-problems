-- Solution for https://www.hackerrank.com/challenges/alternating-characters/problem

module Main where

import Data.List (group)

alternatingCharacters :: String -> Int
alternatingCharacters s = length s - length (group s)

main :: IO ()
main = interact $ unlines . map (show . alternatingCharacters) . tail . lines