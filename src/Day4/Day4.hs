module Day4.Day4 (
) where

type Password = Int

main :: IO ()
main =
  let lowerBound = 235741
      upperBound = 706948
      meetPasswords =
        let allRules = [lengthRule, adjacentRule, increaseRule]
          in filter (meetAllRules lowerBound upperBound) [lowerBound..upperBound]
   in print . length $ meetPasswords


meetAllRules :: Int -> Int -> Password -> Bool
meetAllRules lowerBound upperBound p = all (== True) $ rangeRule lowerBound upperBound : allRules <*> [p]

allRules :: [Password -> Bool]
allRules = [lengthRule, adjacentRule', increaseRule]

numbers :: Password -> [Int]
numbers = fmap readInt . show

lengthRule :: Password -> Bool
lengthRule = (== 6) . length . numbers

rangeRule :: Int -> Int -> Password -> Bool
rangeRule lowerBound upperBound p = p >= lowerBound && p <= upperBound

increaseRule :: Password -> Bool
increaseRule p =
  let adjPair = pairs $ numbers p
      isGreaterThanOrEqual :: (Ord a) => (a, a) ->  Bool
      isGreaterThanOrEqual (x, y) = x <= y
    in all isGreaterThanOrEqual adjPair

adjacentRule :: Password -> Bool
adjacentRule p =
  let adjPair = pairs $ numbers p
      isEqual :: (Eq a) => (a, a) ->  Bool
      isEqual (x, y) = x == y
      pairEqual = fmap isEqual adjPair
      tripleEqual = pairs pairEqual
    in or pairEqual && not (any isEqual tripleEqual)

adjacentRule' :: Password -> Bool
adjacentRule' p =
  let adjPair = pairs $ numbers p
      isEqual :: (Eq a) => (a, a) ->  Bool
      isEqual (x, y) = x == y
      equalPairs = filter isEqual adjPair
   in (not . null $ equalPairs) && elem 1 (fmap (`countElem` equalPairs) equalPairs)

countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (== x)

pairs :: [a] -> [(a, a)]
pairs xs = zip xs $ tail xs

readInt :: Char -> Int
readInt c = read [c]
