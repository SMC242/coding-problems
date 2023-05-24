-- Solution for https://www.hackerrank.com/challenges/mark-and-toys/problem
module Main where

import Data.Function (on)
import Data.List (foldl', maximumBy, sort, subsequences)

toyCombinations :: Int -> [Int] -> [[Int]]
toyCombinations budget prices = filter ((< budget) . sum) . subsequences $ filter (< budget) prices

bestCombination :: [[Int]] -> [Int]
bestCombination = maximumBy (compare `on` length)

foldlIdx :: (b -> (Int, a) -> b) -> b -> [a] -> b
foldlIdx f z xs = foldl' f z (zip [0 ..] xs)

solve :: Int -> [Int] -> Int
solve _ [] = 0
solve budget prices = foldlIdx (\acc (i, x) -> if acc < 0 then i else acc - x) budget prices

main :: IO ()
main = do
  l1 <- getLine
  l2 <- getLine
  let budget = read . (!! 1) $ words l1 :: Int
  let prices = map read . words $ l2 :: [Int]
  print $ solve budget prices