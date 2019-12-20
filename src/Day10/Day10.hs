module Day10.Day10 (
) where

import Combinatorics (tuples)
import Numeric.Extra (intToFloat)
import System.IO (readFile)

type Point = (Int, Int)

main :: IO ()
main = do
  contents <- readFile "src/Day10/input.txt"
  let coors = zip (fmap (zip [0..]) (lines contents)) [0..]
  let points = concatMap transform coors
  let counts = fmap (`countCanSee` points) points
  print $ foldr max 0 counts -- part 1 288

transform :: ([(Int, Char)], Int) -> [Point]
transform (xs, y) = foldl go [] xs
  where go :: [Point] -> (Int, Char) -> [Point]
        go ps (x, '#') = ps ++ [(x, y)]
        go ps _        = ps

countCanSee :: Point -> [Point] -> Int
countCanSee p ps =
  let ps' = removeElem p ps
   in length $ filter (== True) $ fmap (flip (canSee p) ps') ps'

canSee :: Point -> Point -> [Point] -> Bool
canSee p p' ps =
  let ps' = removeElem p . removeElem p' $ ps
      dl = directLine p p'
   in
   case dl of
     []  -> True
     dl' -> not . or $ fmap (`elem` dl') ps'

directLine :: Point -> Point -> [Point]
directLine p@(x, y) p'@(x', y') =
  [
    (m, n) |
      m <- xx'
      , n <- yy',
      -- if the angle between (m, n) and p is same as the angle between p p'
      -- it means that (m, n) is in the line between p and p'
      -- Use epsilon to compare the 2 float value as it cannot be compared
      -- directly
      abs (angle p p' - angle p (m, n)) < epsilon
  ]
  where
    epsilon = 0.0000001
    xx' = if x < x' then [x..x'] else [x'..x]
    yy' = if y < y' then [y..y'] else [y'..y]
    angle p1@(m, n) p2@(m', n') = intToFloat(abs (m - m')) / distance p1 p2

distance :: Point -> Point -> Float
distance (x, y) (x', y') = sqrt . intToFloat $ (x - x')^2 + (y - y')^2

removeElem :: Eq a => a -> [a] -> [a]
removeElem a = filter (/= a)
