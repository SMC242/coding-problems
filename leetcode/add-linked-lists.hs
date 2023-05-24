-- Solution for https://leetcode.com/problems/add-two-numbers/
module Main where

import Data.Char (digitToInt)

-- Digits in reverse order in O(n) time
-- Source: https://stackoverflow.com/a/3963286
link :: Int -> [Int]
link 0 = [0] -- Guard because the inner function will convert this to []
link x = link' x
  where
    link' 0 = []
    link' x = r : link' q where (q, r) = x `quotRem` 10

-- Convert from reversed singly-linked list
unlink :: [Int] -> Int
unlink xs = aux xs 1
  where
    aux :: [Int] -> Int -> Int
    aux [] _ = 0
    aux (x : xs) level = x * level + aux xs (level * 10)

add :: [Int] -> [Int] -> [Int]
add xs ys = link $ unlink xs + unlink ys

main :: IO ()
main = undefined