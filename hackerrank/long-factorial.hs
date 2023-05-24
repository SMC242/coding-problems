-- Solution for https://www.hackerrank.com/challenges/extra-long-factorials/problem
module Main where

bigFactorial :: Integer -> Integer
bigFactorial n = product [1 .. n]

main :: IO ()
main = interact $ show . bigFactorial . read