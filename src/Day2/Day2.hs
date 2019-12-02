module Day2 (
) where

import Data.List.Split
import System.IO (readFile)

type IntCode = Int
type Program = [IntCode]

main :: IO ()
main = do
  program <- fetchProgram
  print . head . rebuild 0 $ setAt (setAt program 1 12) 2 2

fetchProgram :: IO Program
fetchProgram = do
  contents <- readFile "./src/Day2/input.txt"
  pure . fmap (read :: String -> IntCode) . splitOn "," $ contents

rebuild :: Int -> Program -> Program
rebuild currentPos program =
  if optCode == 99
    then program
    else
      let operand1 = program !! (program !! (currentPos + 1))
          operand2 = program !! (program !! (currentPos + 2))
          replacePos = program !! (currentPos + 3)
          nextPos = currentPos + 4
        in
          if optCode == 1
            then rebuild nextPos $ setAt program replacePos (operand1 + operand2)
            else if optCode == 2
                    then rebuild nextPos $ setAt program replacePos (operand1 * operand2)
                    else undefined
  where optCode = program !! currentPos

setAt :: [a] -> Int -> a -> [a]
setAt xs n x =
  let (xs', _:xs'') = splitAt n xs
   in xs' ++ [x] ++ xs''

