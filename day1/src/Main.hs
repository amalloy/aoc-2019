module Main where

import Control.Arrow ((&&&))

fuelCost :: Int -> Int
fuelCost size = (size `div` 3) - 2

part1 :: [Int] -> Int
part1 = sum . map fuelCost

part2 :: [Int] -> ()
part2 s = ()

main :: IO ()
main = interact $ show . (part1 &&& part2) . map read . lines
