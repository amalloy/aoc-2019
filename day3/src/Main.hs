module Main where

import Control.Arrow ((&&&))
import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum(..))
import Data.Ord (comparing)
import Text.Read (readMaybe)

import qualified Data.Map as M

type Input = [[(Int, Int)]]

intermediates :: Monoid a => [a] -> [a]
intermediates = scanl (<>) mempty

asUnits :: Num a => (Int, Int) -> [(a, a)]
asUnits (0, n) | n >= 0 = replicate n (0, 1)
               | otherwise = replicate (negate n) (0, -1)
asUnits (n, 0) | n >= 0 = replicate n (1, 0)
               | otherwise = replicate (negate n) (-1, 0)
asUnits _ = error "Non-Manhattan movement"

readVector :: String -> Maybe (Int, Int)
readVector (dir:magnitude) = do
  mag <- readMaybe magnitude
  case dir of
    'L' -> pure (mag, 0)
    'R' -> pure (- mag, 0)
    'U' -> pure (0, mag)
    'D' -> pure (0, - mag)
    _ -> Nothing
readVector _ = Nothing

parse :: String -> [(Int, Int)]
parse = mapMaybe readVector . splitOn ","

visited :: Input -> [M.Map (Sum Int, Sum Int) Int]
visited wires = do
  wire <- wires
  let units = asUnits =<< wire
      spacesVisited = tail . flip zip [0..] . intermediates $ units
  pure . M.fromListWith const $ spacesVisited

part1 :: Input -> (Sum Int, Sum Int)
part1 wires = let traces = visited wires
              in minimumBy (comparing (\(x, y) -> abs x + abs y))
                 . M.keys . foldr1 M.intersection $ traces

part2 :: Input -> Int
part2 = minimum . map snd . M.assocs . foldr1 (M.intersectionWith (+)) . visited

prepare :: String -> Input
prepare = map parse . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
