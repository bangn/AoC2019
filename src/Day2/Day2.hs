module Day2 (
) where

import Data.List.Split
import System.IO (readFile)

type IntCode = Int
type Program = [IntCode]

main :: IO ()
main = do
  program <- fetchProgram
  let result = rebuild (0, setAt (setAt program 1 12) 2 2)
  print . head . snd $ result
  let (noun, verb) = findNounAndVerb program 19690720
  print $ 100 * noun + verb

findNounAndVerb :: Program -> Int -> (Int, Int)
findNounAndVerb program expected =
  head [
    (noun, verb) |
      noun <- [0..99],
      verb <- [0..99],
        let result = rebuild (0, setAt (setAt program 1 noun) 2 verb)
          in
            (head . snd $ result) == expected
 ]


fetchProgram :: IO Program
fetchProgram = do
  contents <- readFile "./src/Day2/input.txt"
  pure . fmap (read :: String -> IntCode) . splitOn "," $ contents

rebuild :: (Int, Program) -> (Int, Program)
rebuild (ip, program)=
  if optCode == 99
    then (ip + 1, program)
    else
      let operand1 = program !! (program !! (ip + 1))
          operand2 = program !! (program !! (ip + 2))
          replacePos = program !! (ip + 3)
          nextPos = ip + 4
        in
          if optCode == 1
            then rebuild (nextPos, setAt program replacePos $ operand1 + operand2)
            else if optCode == 2
                    then rebuild (nextPos, setAt program replacePos $ operand1 * operand2)
                    else undefined
  where optCode = program !! ip

setAt :: [a] -> Int -> a -> [a]
setAt xs n x =
  let (xs', _:xs'') = splitAt n xs
   in xs' ++ [x] ++ xs''

