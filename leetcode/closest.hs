-- Solution for https://leetcode.com/problems/k-closest-points-to-origin/
module Main where

import Data.Foldable (minimumBy)
import Data.Ord (comparing)

data Point = Point {pointX :: Float, pointY :: Float, pointZ :: Float} deriving (Show)

distance :: Point -> Point -> Float
distance x y = sqrt $ (pointX x - pointX y) ** 2 + (pointY x - pointY y) ** 2 + (pointZ x - pointZ y) ** 2

closest :: Point -> [Point] -> Point
closest origin ps = fst $ minimumBy (comparing snd) $ zip ps (map (distance origin) ps)

closest' :: Point -> [Point] -> Point
closest' origin = minimumBy (comparing (distance origin))

points :: [Point]
points =
  [ Point 1 0 4,
    Point 2 0 2,
    Point 3 0 0
  ]

main :: IO ()
main = print $ closest' (Point 0 0 0) points