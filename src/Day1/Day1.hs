module Day1 () where

import System.IO (readFile)

type Mass = Int
type Fuel = Int

main :: IO ()
main = do
  masses <- fetchModules "./src/Day1/input.txt"
  print . sum . fmap fuelRequired $ masses

fetchModules :: String -> IO [Mass]
fetchModules input = do
  contents <- readFile input
  pure . fmap readInt . words $ contents

fuelRequired :: Mass -> Fuel
fuelRequired m = if frm <= 0
                    then 0
                    else frm + fuelRequired frm
                      where frm = m `div` 3 - 2

readInt :: String -> Mass
readInt = read
