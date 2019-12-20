module Day10.Day10 (
) where

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
      cotan p p' == cotan p (m, n)
  ]
  where
    xx' = if x < x' then [x..x'] else [x'..x]
    yy' = if y < y' then [y..y'] else [y'..y]
    cotan p1@(m, n) p2@(m', n') = toRational (fromIntegral (abs (m - m')) / fromIntegral (abs (n - n')))

removeElem :: Eq a => a -> [a] -> [a]
removeElem a = filter (/= a)
