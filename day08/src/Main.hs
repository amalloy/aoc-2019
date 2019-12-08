module Main where

import Data.List (minimumBy, transpose)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)

type Input = [String]

width, height :: Int
width = 25
height = 6

part1 :: Input -> Int
part1 layers = let count n = (length . filter (== n))
                   bestLayer = minimumBy (comparing (count '0')) layers
          in count '1' bestLayer * count '2' bestLayer

part2 :: Input -> String
part2 layers = let pixels = transpose layers
                   image = map (head . dropWhile (== '2')) pixels
               in unlines . chunksOf width $ image

prepare :: String -> Input
prepare = chunksOf (width * height) . init

main :: IO ()
main = do
  input <- prepare <$> readFile "input.txt"
  print $ part1 input
  putStr $ part2 input
