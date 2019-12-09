module Main where

import Advent.Intcode

import Control.Arrow ((&&&))

type Input = Program

part1 :: Input -> Either String [Value]
part1 = fmap outputs . runToCompletion . (`mkComputer` [1])

part2 :: Input -> Either String [Value]
part2 = fmap outputs . runToCompletion . (`mkComputer` [2])

prepare :: String -> Input
prepare = parseIntcodeProgram

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
