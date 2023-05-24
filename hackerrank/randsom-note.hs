-- Solution for https://www.hackerrank.com/challenges/ctci-ransom-note/problem
module Main where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, maybe)

data Response = Yes | No deriving (Show)

type Counter = M.Map String Int

wordOccurrences :: String -> Counter
wordOccurrences s = M.fromListWith (+) $ zip ws (map (const 1) ws) where ws = words s

checkMagazine' :: Counter -> Counter -> Bool
checkMagazine' mag note = all ((fromMaybe False . enough) . (\(k, x) -> (x, M.lookup k mag))) (M.assocs note)
  where
    enough :: (Int, Maybe Int) -> Maybe Bool
    enough (x, y) = (>= x) <$> y

listTo2Tuple :: [a] -> Maybe (a, a)
listTo2Tuple [] = Nothing
listTo2Tuple (a : b : _) = Just (a, b)
listTo2Tuple [_] = Nothing

response :: Bool -> Response
response x = if x then Yes else No

main :: IO ()
main = interact $ \x -> show . response . maybe False (uncurry checkMagazine') $ listTo2Tuple . map wordOccurrences . tail . lines $ x