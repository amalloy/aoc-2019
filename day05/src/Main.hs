module Main where

import qualified Intcode as I

import Data.List.Split (splitOn)

import Control.Arrow ((&&&))

type Input = [Int]

run :: [Int] -> Input -> Either String [Int]
run inputs mem = fmap I.outputs . I.runToCompletion $ I.mkComputer mem inputs

part1 :: Input -> Either String [Int]
part1 = run [1]

part2 :: Input -> Either String [Int]
part2 = run [5]

prepare :: String -> Input
prepare = map read . splitOn "," . head . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
