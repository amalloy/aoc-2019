module Main where

import Advent.Intcode

import Control.Arrow ((&&&))
import Control.Monad (foldM)
import Data.List (permutations)

type Input = Program

runAmplifier :: Program -> Int -> Int -> Either String Int
runAmplifier program input phase = head . outputs <$> runToCompletion computer
  where computer = mkComputer program [phase, input]

part1 :: Input -> Either String Int
part1 p = fmap maximum . sequence $ do
  phases <- permutations [0..4]
  pure $ foldM (runAmplifier p) 0 phases

part2 :: Input -> ()
part2 i = ()

prepare :: String -> Input
prepare = parseIntcodeProgram

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
