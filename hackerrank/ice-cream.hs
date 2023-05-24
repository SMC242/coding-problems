-- Solution for https://www.hackerrank.com/challenges/ctci-ice-cream-parlor/problem
module Main where

import Control.Monad (forM_, join)
import Data.Function (on)
import Data.List (find, nubBy, sort)
import Data.Map.Lazy as M (Map, fromList, keys, lookup)
import Data.Maybe (catMaybes, isJust)

solve :: Int -> [Int] -> [Int]
solve budget prices = case join . find isJust . map (tryKey menu) $ M.keys menu of
  Nothing -> [-1, -1]
  Just k -> sort . catMaybes $ [M.lookup k menu, M.lookup (budget - k) menu]
  where
    -- Map of (price: id)
    menu = M.fromList $ zip prices [1 ..]
    tryKey :: M.Map Int Int -> Int -> Maybe Int
    tryKey m k = case M.lookup (budget - k) m of
      Nothing -> Nothing
      Just id -> Just k

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1 .. t] $ \_ -> do
    budget <- readLn :: IO Int
    _ <- getLine
    items <- getLine
    putStrLn . unwords . map show $ solve budget (map read . words $ items :: [Int])
