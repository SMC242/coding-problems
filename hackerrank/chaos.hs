-- https://www.hackerrank.com/challenges/new-year-chaos/problem
import Data.List (zipWith)
import Data.Maybe (maybe)

test1 :: [Int]
test1 = [1, 2, 3, 5, 4, 6, 7, 8]

test2 :: [Int]
test2 = [4, 1, 2, 3]

test3 :: [Int]
test3 = [1, 2, 4, 3, 5, 8, 6, 7]

test4 :: [Int]
test4 = [1, 2, 4, 3, 7, 8, 6, 4]

{-
Does not work because this solution doesn't account for this situation:
1 2 3 4 5 6 7 8 (0 bribes)
1 2 5 3 4 6 7 8 (2 bribes)
1 2 5 3 7 4 6 8 (4 bribes)
1 2 4 3 7 8 4 6 (6 bribes)
1 2 4 3 7 8 6 4 (7 bribes)

Solution: make a window between x and its index, then use `bribesInWindow`.

Source: https://csanim.com/tutorials/hackerrank-solution-new-year-chaos
-}
bribes :: [Int] -> [Int]
bribes q = filter (> 0) $ zipWith (-) q [1 ..]

-- WIP: solution using windows
bribesInWindow :: Int -> [Int] -> Int
bribesInWindow x win = length $ filter (> x) win

slice :: Int -> Int -> [a] -> [a]
slice l r xs
  | l >= r || any (< 0) [l, r] = []
  | otherwise = take (r - l + 1) . drop l $ xs

bribes' :: [Int] -> [Int]
bribes' q = zipWith countBribes q [0 ..]
  where
    offsets = zipWith (-) q [1 ..]
    countBribes x o = bribesInWindow x (slice (max 0 o) (x - 1) q) -- Prevent negative index in o and convert x to 0-based index

-- Translating https://csanim.com/tutorials/hackerrank-solution-new-year-chaos
bribes'' :: [Int] -> [Int]
bribes'' q = zipWith (\o i -> length . filter (> o) $ slice (o - 1) i q) q [0 ..]

solve :: [Int] -> Maybe Int
solve q = if any (> 2) bs then Nothing else Just $ sum bs where bs = bribes' q

main :: IO ()
main =
  interact $
    unlines
      . map (maybe "Too chaotic" show . solve . map read . words)
      . filter ((> 1) . length)
      . lines
