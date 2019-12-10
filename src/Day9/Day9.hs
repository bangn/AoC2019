module Day5 (
) where

import Data.List.Split
import Data.Maybe (fromMaybe)
import System.IO (readFile)

type InstructionCounter = Int
type Program = (InstructionCounter, [Int])
type App = (Int, [Int], Program)

data Opcode = NoOp | Plus | Mulitply | In | Out | JIT | JIF | Lt | Eq | Stop
  deriving (Eq, Enum, Ord, Show)

main :: IO ()
main = do
  codes <- fetchCodes "./src/Day5/input.txt"
  let (input, output1, (nextIc, result)) = run (1, [], (0, codes))
  print output1
  let (input, output2, (nextIc, result)) = run (5, [], (0, codes))
  print output2

fetchCodes :: String -> IO [Int]
fetchCodes input = do
  contents <- readFile input
  pure . fmap (read :: String -> Int) . splitOn "," $ contents

run :: App -> App
run (input, output, p@(ic, codes)) =
    let (opcode, modes, inOffs, outOffs) = decode $ getAt p ic 0
        ins = fmap (getAt p ic) inOffs
        outs = fmap (getAt p ic) outOffs
        args = getAtM p <$> zip modes ins
        (output', codes', maybeNextIc) = exec opcode args outs
        nextIc = fromMaybe (ic + length inOffs + length outOffs + 1) maybeNextIc
     in if opcode == Stop
        then (input, output, (nextIc, codes'))
        else run (input, output', (nextIc, codes'))
  where
    exec :: Opcode -> [Int] -> [Int] -> ([Int], [Int], Maybe InstructionCounter)
    exec op args outs =
      case op of
       Plus     -> (output, setAt codes (head outs) (sum args), Nothing)
       Mulitply -> (output, setAt codes (head outs) (product args), Nothing )
       In       -> (output, setAt codes (head outs) input, Nothing)
       Out      -> (head args : output, codes, Nothing)
       JIT -> let (f:s:_) = args
                  mni = if f /= 0 then Just s else Nothing
               in (output, codes, mni)
       JIF -> let (f:s:_) = args
                  mni = if f == 0 then Just s else Nothing
               in (output, codes, mni)
       Lt -> let (f:s:_) = args
                 v = if f < s then 1 else 0
               in (output, setAt codes (head outs) v, Nothing)
       Eq -> let (f:s:_) = args
                 v = if f == s then 1 else 0
               in (output, setAt codes (head outs) v, Nothing)
       _        -> (output, codes, Nothing)

decode :: Int -> (Opcode, [Int], [Int], [Int])
decode n =
    let op = opCode n
    in (op, modes n, fst (inout op), snd (inout op))
  where
    inout :: Opcode -> ([Int], [Int])
    inout op = case op of
      Plus     -> ([1, 2], [3])
      Mulitply -> ([1, 2], [3])
      In       -> ([],     [1])
      Out      -> ([1],    [])
      JIT      -> ([1, 2], [])
      JIF      -> ([1, 2], [])
      Lt       -> ([1, 2], [3])
      Eq       -> ([1, 2], [3])
      _        -> ([],     [])

    opCode :: Int -> Opcode
    opCode 99 = Stop
    opCode x  = toEnum $ x `mod` 100

    modes :: Int -> [Int]
    modes x = fmap digitToMode [ x `div` 100      -- first param mode
                               , x `div` 1000     -- second param mode
                               , x `div` 10000 ]  -- third param mode

    digitToMode :: Int -> Int
    digitToMode = fromEnum . odd

getAt :: Program -> Int -> Int -> Int
getAt (_, codes) ic off = codes !! (ic + off)

getAtM :: Program -> (Int, Int) -> Int
getAtM (_, codes) (mode, pos) =
  if mode == 0
     then codes !! pos
     else pos

setAt :: [a] -> Int -> a -> [a]
setAt xs n x =
  let (xs', _:xs'') = splitAt n xs
   in xs' ++ [x] ++ xs''
