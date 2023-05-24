-- Solution for https://www.hackerrank.com/challenges/luck-balance/problem
module Main where

import Control.Monad (forM)
import Data.Function
import Data.List
import Data.Maybe (mapMaybe)

type Contest = (Int, Int)

contests :: [Contest]
contests = [(5, 1), (1, 1), (4, 0)]

sumBy :: (Foldable t, Num b) => (a -> b) -> t a -> b
sumBy f = foldr ((+) . f) 0

sumLuck :: [Contest] -> Int
sumLuck = sumBy fst

filterPriority :: (Int -> Bool) -> [Contest] -> [Contest]
filterPriority f = filter (f . snd)

luckFloor :: [Contest] -> Int
luckFloor = sumLuck . filterPriority (== 0)

maxLuck :: Int -> [Contest] -> Int
maxLuck n = sumLuck . take n . sortBy (flip compare `on` fst) . filterPriority (== 1)

solve :: Int -> [Contest] -> Int
solve n cs = maxLuck n cs + luckFloor cs

to2Tuple :: [a] -> Maybe (a, a)
to2Tuple [x, y] = Just (x, y)
to2Tuple _ = Nothing

main :: IO ()
main = do
  --   l1 <- map read . words <$> getLine :: IO [Int]
  --   let k = l1 !! 1
  --   let contests = forM [0 .. n] (\_ ->  mapMaybe (to2Tuple . read) . words <$> getLine :: IO [(Int, Int)])
  contents <- lines <$> getContents
  let n = read . (!! 1) . words . head $ contents :: Int
  let contests = mapMaybe (to2Tuple . map read . words) $ tail contents :: [(Int, Int)]
  print $ solve n contests
