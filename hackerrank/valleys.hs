-- Solution for https://www.hackerrank.com/challenges/counting-valleys/problem
import Data.List (group, groupBy)

valleys :: String -> Int
valleys path = length . filter head . group . map (< 0) $ scanl (\acc x -> if x == 'U' then acc + 1 else acc - 1) 0 path

solve :: IO ()
solve = interact $ show . valleys . (!! 1) . lines