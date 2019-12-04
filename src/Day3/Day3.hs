module Day3 (
) where

import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Set as S
import System.IO (readFile)

newtype Point = Point (Int, Int)
  deriving (Eq, Ord, Show)

type Wire = [Point]

data Direction = U | R | D | L
  deriving (Eq, Ord, Show)

type Distance = Int

type Path = (Direction, Distance)

main :: IO ()
main = do
  input <- readFile "./src/Day3/input.txt"
  let paths = fmap (fmap mkPath . splitOn ",") . lines $ input
      ws = unWires . wires $ paths
      ins = intersections ws
      distanceToClosestIntersection = minimum . fmap (matDistance startP) $ ins
      sps = splitAt (length ins) $ stepsTo <$> ws <*> ins
      -- include start and endpoint
      minimumStepsToIntersections = (+2) . minimum . fmap sumSteps $ uncurry zip sps
    in print (distanceToClosestIntersection, minimumStepsToIntersections)

sumSteps :: (Int, Int) -> Int
sumSteps (x, y) = x + y

startP = Point (0, 0)
startW = [] :: Wire

intersections :: [Wire] -> [Point]
intersections ws = S.toList . S.intersection (S.fromList . head $ ws) $ (S.fromList . last $ ws)

stepsTo :: Wire -> Point -> Int
stepsTo ps p = length $ takeWhile (/= p) ps

wires :: [[Path]] -> [(Point, Wire)]
wires = fmap (foldl mkLine (startP, startW))

unWires :: [(Point, Wire)] -> [Wire]
unWires = fmap snd

matDistance :: Point -> Point -> Int
matDistance (Point (x, y)) (Point (x', y')) = abs(x - x') + abs(y - y')

mkLine :: (Point, Wire) -> Path -> (Point, Wire)
mkLine (Point (x, y), w) (d, l) =
  case d of
    U -> (Point (x, y + l), w ++ (Point . (x,) <$> go (+ 1) y l))
    R -> (Point (x + l, y), w ++ (Point . (,y) <$> go (+ 1) x l))
    D -> (Point (x, y - l), w ++ (Point . (x,) <$> go (flip (-) 1) y l))
    L -> (Point (x - l, y), w ++ (Point . (,y) <$> go (flip (-) 1) x l))
  where go :: (a -> a) -> a -> Int -> [a]
        go f start length = take length $ iterate f (f start)

mkPath :: String -> Path
mkPath (d:l) =
  case d of
    'U' -> (U, readInt l)
    'R' -> (R, readInt l)
    'D' -> (D, readInt l)
    'L' -> (L, readInt l)

readInt :: String -> Int
readInt = read
