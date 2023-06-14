-- Solution for https://leetcode.com/problems/two-sum/description/

import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe, maybe)
import Tools.ParseLeetcode qualified as P

twoSum :: Integral a => a -> [a] -> Maybe (Int, Int)
twoSum n xs = M.lookup x m >>= (Just . (,idx))
  where
    m = M.fromList $ zip xs [0 ..] -- Map of (x, index)
    (x, idx) = (!! 0) $ zip xs (mapMaybe (flip M.lookup m . (n -)) xs)

verify :: Integral a => a -> [a] -> (Int, Int) -> Bool
verify n xs (i1, i2) = xs !! i1 + xs !! i2 == n

parseProblem :: P.Parser ([Integer], Integer)
parseProblem = (,) <$> P.variableOf' (P.listOf P.int) <* P.commaSeparator <*> P.variableOf' P.int

main :: IO ()
main = do
  parsed <- getLine >>= P.parseQuestionM parseProblem
  traverse_
    ( \(nums, target) -> do
        let result = twoSum target nums
        case result of
          Just idxs -> do
            print idxs
            putStrLn $ "Valid? " ++ show (verify target nums idxs)
          Nothing -> print "No result"
        return ()
    )
    parsed