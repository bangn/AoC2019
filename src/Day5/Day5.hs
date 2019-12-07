module Day5 (
) where

import Data.List.Split
import System.IO (readFile)

type Program = [Int]

main :: IO ()
main = do
  program <- fetchProgram "./src/Day2/input.txt"
  let result = rebuild (0, setAt (setAt program 1 12) 2 2)
  print . head . snd $ result
  let (noun, verb) = findNounAndVerb program 19690720
  print $ 100 * noun + verb

fetchProgram :: String -> IO Program
fetchProgram input = do
  contents <- readFile input
  pure . fmap (read :: String -> Int) . splitOn "," $ contents

run :: (Int, Program) -> (Int, Program)
run (ip, program) =
  let
    (optCode, op1, op2) = getOpcodeAndOperans ip program
    replacePos = program !! (ip + 3)
   in case optCode of
        99 -> (ip + 1, program)
        1  -> run(ip + 4, setAt program replacePos $ op1 + op2)
        2  -> run(ip + 4, setAt program replacePos $ op1 * op2)
        _  -> undefined

getOpcodeAndOperans :: Int -> Program -> (Int, Int, Int)
getOpcodeAndOperans ip program = (optCode, operand1, operand2)
  where optCode = program !! ip
        operand1 = program !! (program !! (ip + 1))
        operand2 = program !! (program !! (ip + 2))

setAt :: [a] -> Int -> a -> [a]
setAt xs n x =
  let (xs', _:xs'') = splitAt n xs
   in xs' ++ [x] ++ xs''

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

