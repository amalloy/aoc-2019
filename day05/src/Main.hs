module Main where

import qualified Intcode as I

import Control.Arrow ((&&&))

type Input = [String]

part1 :: Input -> ()
part1 i = ()

part2 :: Input -> ()
part2 i = ()

prepare :: String -> Input
prepare = lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
