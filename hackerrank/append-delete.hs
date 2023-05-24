-- Solution for https://www.hackerrank.com/challenges/append-and-delete/problem
module Main where

import Data.Set qualified as S

countChanges :: S.Set Char -> S.Set Char -> Int
countChanges x y = S.size (S.difference x y) + S.size (S.difference y x)

zipOrRepeat :: [a] -> [a] -> [(a, a)]
zipOrRepeat [] [] = []
zipOrRepeat (x : xs) (y : ys) = (x, y) : zipOrRepeat xs ys
zipOrRepeat (x : xs) [] = (x, x) : zipOrRepeat xs []
zipOrRepeat [] (y : ys) = (y, y) : zipOrRepeat [] ys

zipWithPadding :: a -> b -> [a] -> [b] -> [(a, b)]
zipWithPadding a b (x : xs) (y : ys) = (x, y) : zipWithPadding a b xs ys
zipWithPadding a _ [] ys = zip (repeat a) ys
zipWithPadding _ b xs [] = zip xs (repeat b)

dropNotMatching :: String -> String -> String
dropNotMatching x y = map fst . dropWhile (uncurry (/=)) $ zipWithPadding ' ' ' ' (reverse x) (reverse y)

solve :: String -> String -> Int -> Bool
solve s t k = undefined
  where
    (sSet, tSet) = (S.fromList s, S.fromList t)
    toRemove = S.difference sSet tSet
    toAdd = S.difference tSet sSet

main :: IO ()
main = undefined