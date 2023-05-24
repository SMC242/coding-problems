-- Solution for https://www.hackerrank.com/challenges/sherlock-and-valid-string/problem
module Main where

import qualified Data.Map as M

countOccurrences :: Ord a => [a] -> M.Map a Int
countOccurrences xs = M.fromListWith (+) [(x, 1) | x <- xs]

sumMap :: Num a => M.Map k a -> a
sumMap = M.foldr (+) 0

valid :: String -> Bool
valid s = (<= target + 1) $ sum $ filter (> target) xs
  where
    xs = M.elems (countOccurrences s)
    target = minimum xs

yesOrNo :: Bool -> String
yesOrNo b = if b then "YES" else "NO"

main :: IO ()
main = interact $ yesOrNo . valid