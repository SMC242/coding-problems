-- Solution for https://www.hackerrank.com/challenges/minimum-absolute-difference-in-an-array/problem?
module Main where

import Data.List (sort)

minimumAbsoluteDifference :: [Int] -> Int
minimumAbsoluteDifference = minimum . map (\(x, y) -> abs (x - y)) . (\xs -> zip xs (drop 1 xs)) . sort

main :: IO ()
main = interact $ show . minimumAbsoluteDifference . map read . words . (!! 1) . lines