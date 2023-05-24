-- Solution for https://leetcode.com/problems/snail-traversal/
module Main where

import Data.List (transpose)
import Data.Text qualified as T

type Table a = [[a]]

testData :: [Int]
testData = [19, 10, 3, 7, 9, 8, 5, 2, 1, 17, 16, 14, 12, 18, 6, 13, 11, 20, 4, 15]

showTable :: Show a => Table a -> String
showTable = unlines . map show

chunk :: Int -> [a] -> Table a
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

snailOrder :: Int -> [a] -> Table a
snailOrder rows = transpose . chunk rows

parseVariable :: Read a => String -> Maybe a
parseVariable s
  | length xs == 1 = Nothing -- No equals sign
  | otherwise = Just . read . T.unpack . T.strip $ xs !! 1 -- NOTE: Discards chained equals
  where
    xs = T.split (== '=') $ T.pack s

parseLine :: Read a => IO (Maybe a)
parseLine = getLine >>= pure . parseVariable

main :: IO ()
main = do
  Just nums <- parseLine :: IO (Maybe [Int])
  Just rows <- parseLine :: IO (Maybe Int)
  putStr . showTable $ snailOrder rows nums