module Main where

import Control.Arrow ((&&&))
import Data.Functor.Foldable (ana, ListF(..))

fuelCost :: Int -> Int
fuelCost mass = max 0 $ (mass `div` 3) - 2

part1 :: [Int] -> Int
part1 = sum . map fuelCost

tyrannicalCost :: Int -> [Int]
tyrannicalCost 0 = []
tyrannicalCost mass = fuel : tyrannicalCost fuel
  where fuel = fuelCost mass

tyrannicalCost' :: Int -> [Int]
tyrannicalCost' = ana f
  where f 0 = Nil
        f mass = Cons fuel fuel
          where fuel = fuelCost mass

part2 :: [Int] -> Int
part2 = sum . concatMap tyrannicalCost'

main :: IO ()
main = interact $ show . (part1 &&& part2) . map read . lines
