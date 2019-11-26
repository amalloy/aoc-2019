module Main where

import Control.Arrow ((&&&))

part1 :: String -> ()
part1 s = ()

part2 :: String -> ()
part2 s = ()

main :: IO ()
main = interact $ show . (part1 &&& part2)
