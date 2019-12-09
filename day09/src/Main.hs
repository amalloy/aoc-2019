module Main where

import Advent.Intcode

import Control.Arrow ((&&&))

type Input = Computer

part1 :: Input -> Either String [Value]
part1 = fmap outputs . runToCompletion

part2 :: Input -> ()
part2 i = ()

prepare :: String -> Input
prepare = (`mkComputer` [1]) . parseIntcodeProgram

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
