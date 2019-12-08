module Main where

import Control.Arrow ((&&&))
import Data.List (minimumBy)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)

type Input = String

width, height :: Int
width = 25
height = 6

part1 :: Input -> Int
part1 s = let layers = chunksOf (width * height) s
              count n = (length . filter (== n))
              bestLayer = minimumBy (comparing (count '0')) layers
          in count '1' bestLayer * count '2' bestLayer

part2 :: Input -> ()
part2 i = ()

prepare :: String -> Input
prepare = init

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
