module Day5 (
) where

import Data.List.Split
import System.IO (readFile)

type InstructionCounter = Int
type OptCode = Int
type Program = (InstructionCounter, OptCode, [Int])
type State = (Int, [Int], Program)

main :: IO ()
main = do
  codes <- fetchCodes "./src/Day2/input.txt"
  let (_, output, (_, _, result)) = run (1, [], (0, head codes, setAt (setAt codes 1 12) 2 2))
  print output

fetchCodes :: String -> IO [Int]
fetchCodes input = do
  contents <- readFile input
  pure . fmap (read :: String -> Int) . splitOn "," $ contents

run :: State -> State
run (input, output, (ic, 99, codes)) = (input, output, (ic + 1, 99, codes))
run (input, output, p@(ic, optCode, codes)) =
  case optCode of
    1 -> let nextIc = ic + 4
             nextOptCode = getAt p nextIc
             replacePos = getAt p (nextIc - 1)
             operand1 = getAt p $ getAt p (ic + 1)
             operand2 = getAt p $ getAt p (ic + 2)
          in run (input, output, (nextIc, nextOptCode, setAt codes replacePos (operand1 + operand2)))
    2 -> let nextIc = ic + 4
             nextOptCode = getAt p nextIc
             replacePos = getAt p (nextIc - 1)
             operand1 = getAt p $ getAt p (ic + 1)
             operand2 = getAt p $ getAt p (ic + 2)
          in run (input, output, (nextIc, nextOptCode, setAt codes replacePos (operand1 * operand2)))
    3 -> let nextIc = ic + 2
             replacePos = getAt p (nextIc - 1)
             nextOptCode = getAt p nextIc
          in run (input, output, (nextIc, nextOptCode, setAt codes replacePos input))
    4 -> let nextIc = ic + 2
             nextOptCode = getAt p nextIc
             outputValue = getAt p (getAt p (ic + 1))
          in run (input, output ++ [outputValue], (nextIc, nextOptCode, codes))

getAt :: Program -> Int -> Int
getAt (_, _, codes) at = codes !! at

setAt :: [a] -> Int -> a -> [a]
setAt xs n x =
  let (xs', _:xs'') = splitAt n xs
   in xs' ++ [x] ++ xs''
