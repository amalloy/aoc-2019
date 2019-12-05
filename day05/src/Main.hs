module Main where

import qualified Intcode as I

import Data.List.Split (splitOn)

import Control.Arrow ((&&&))

type Input = [Int]

part1 :: Input -> Either String [Int]
part1 mem = (fmap I.outputs) . I.runToCompletion $ I.mkComputer mem [1]

part2 :: Input -> ()
part2 i = ()

prepare :: String -> Input
prepare = map read . splitOn "," . head . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
