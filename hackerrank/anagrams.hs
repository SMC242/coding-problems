-- Solution for https://www.hackerrank.com/challenges/ctci-making-anagrams/problem

module Main where

import qualified Data.Map as M

type Counter = M.Map Char Int

charOccurrences :: String -> Counter
charOccurrences s = M.fromListWith (+) $ zip s (map (const 1) s)

{-
    if k in a but not b: remove
    if k in b but not a: remove
    if k in b and C(a[k]) >
-}

sumMap :: Num a => M.Map k a -> a
sumMap = M.foldr (+) 0

solve :: String -> String -> Int
solve a b = (mag -) . (* 2) . sumMap $ M.intersectionWith (\x y -> if x /= y then min x y else x) ca cb
  where
    ca = charOccurrences a
    cb = charOccurrences b
    mag = sumMap ca + sumMap cb

main :: IO ()
main = do
  l1 <- getLine
  l2 <- getLine
  print (solve l1 l2)