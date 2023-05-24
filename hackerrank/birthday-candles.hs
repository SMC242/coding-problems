-- Solution for https://www.hackerrank.com/challenges/birthday-cake-candles/problem
module Main where

birthdayCakeCandles :: [Int] -> Int
birthdayCakeCandles xs = length $ filter (== m) xs where m = maximum xs

main :: IO ()
main = interact $ show . birthdayCakeCandles . map read . words . (!! 1) . lines