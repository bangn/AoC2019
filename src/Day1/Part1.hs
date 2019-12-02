module Day1 () where

import System.IO (readFile)

type Mass = Int
type Fuel = Int

main :: IO ()
main = do
  masses <- modules
  print . sum . fmap fuelRequired $ masses

modules :: IO [Mass]
modules = do
  contents <- readFile "./src/Day1/part1Input.txt"
  pure . fmap readInt . words $ contents

fuelRequired :: Mass -> Fuel
fuelRequired m = m `div` 3 - 2

readInt :: String -> Mass
readInt = read
