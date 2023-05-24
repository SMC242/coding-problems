{-# LANGUAGE OverloadedStrings #-}

-- Solution for https://leetcode.com/problems/two-sum/description/

import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Text qualified as T

twoSum :: Int -> [Int] -> Maybe (Int, Int)
twoSum n xs = case M.lookup x m of
  Just i -> Just (i, idx)
  Nothing -> Nothing -- Should be impossible because the result was already found
  where
    m :: Map Int Int
    m = M.fromList $ zip xs [0 ..] -- Map of (x, index)
    (x, idx) = (!! 0) $ zip xs (mapMaybe (flip M.lookup m . (n -)) xs)

verify :: Int -> [Int] -> (Int, Int) -> Bool
verify n xs (i1, i2) = xs !! i1 + xs !! i2 == n

parseVariable :: Read a => T.Text -> Maybe a
parseVariable s
  | length xs == 1 = Nothing -- No match
  | otherwise = Just . read . T.unpack . T.strip . (!! 1) $ xs
  where
    xs = T.split (== '=') s

main :: IO ()
main = do
  line <- getLine
  let (left, right) = T.breakOnEnd "," . T.pack $ line
  if T.length left == 0
    then error "Invalid format"
    else do
      let Just xs = parseVariable . T.dropEnd 1 $ left :: Maybe [Int]
      let Just target = parseVariable right :: Maybe Int
      let result = twoSum target xs
      case result of
        Just idxs -> do
          print idxs
          putStrLn $ "Valid? " ++ show (verify target xs idxs)
        Nothing -> print "No result"
