module Main where

import Control.Arrow ((&&&))
import Data.Functor.Foldable (ana, ListF(..))

type Input = [Int]

fuelCost :: Int -> Int
fuelCost mass = max 0 $ (mass `div` 3) - 2

part1 :: Input -> Int
part1 = sum . map fuelCost

tyrannicalCost :: Int -> [Int]
tyrannicalCost 0 = []
tyrannicalCost mass = fuel : tyrannicalCost fuel
  where fuel = fuelCost mass

-- Just a version of tyrannicalCost using recursion-schemes for practice
tyrannicalCost' :: Int -> [Int]
tyrannicalCost' = ana f
  where f 0 = Nil
        f mass = Cons fuel fuel
          where fuel = fuelCost mass

part2 :: Input -> Int
part2 = sum . concatMap tyrannicalCost'

prepare :: String -> Input
prepare = map read . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
