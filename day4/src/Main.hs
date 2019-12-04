module Main where

import Control.Arrow ((&&&))
import Data.Char (isSpace)
import Data.List (group)

type Input = (Int, Int)

part1 :: Input -> Int
part1 (lo, hi) = length [n | n <- [lo..hi],
                         let pairs = zip <*> tail $ show n,
                         any (uncurry (==)) pairs,
                         all (uncurry (<=)) pairs]

part2 :: Input -> Int
part2 (lo, hi) = length [n | n <- [lo..hi],
                         let pairs = zip <*> tail $ show n,
                         all (uncurry (<=)) pairs,
                         any ((== 2) . length) $ group (show n)]

prepare :: String -> Input
prepare input = case break (== '-') . filter (not . isSpace) $ input of
  (a, (_:b)) -> (read a, read b)
  _ -> error "Malformed input"

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
