-- Solution for https://www.hackerrank.com/challenges/ctci-array-left-rotation/problem
module Main where

-- Using `mod` to handle large `n`s
rotate :: Int -> [a] -> [a]
rotate n xs = take l . drop (mod n l) $ cycle xs
  where
    l = length xs

main :: IO ()
main = do
  l1 <- getLine
  l2 <- getLine
  let d = (!! 1) . map read $ words l1 :: Int
  let xs = words l2
  putStrLn . unwords $ rotate d xs