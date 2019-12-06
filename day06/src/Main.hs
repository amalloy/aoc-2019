module Main where

import Control.Arrow ((&&&))

import qualified Data.Map as M

data Mass a = Mass a String [Mass a]
type ParentMap a = M.Map String (String, a)

type Input = M.Map String [String]

parse :: String -> (String, String)
parse s = let (a, (_:b)) = break (== ')') s
          in (a, b)

addAll :: [(String, String)] -> ParentMap ()
addAll masses = M.fromList $ do
  (center, outer) <- masses
  pure (outer, (center, ()))

invert :: ParentMap a -> M.Map String [String]
invert m = M.fromListWith (++) $ do
  (k, (v, _)) <- M.assocs m
  pure (v, [k])

part1 :: Input -> Integer
part1 m = totalOrbits "COM"
  where totalOrbits k = case M.lookup k m of
          Nothing -> 0
          Just children -> sum [totalOrbits c + totalNodes c | c <- children]
        totalNodes k = case M.lookup k m of
          Nothing -> 1
          Just xs -> 1 + sum [totalNodes x | x <- xs]

part2 :: Input -> ()
part2 i = ()

prepare :: String -> Input
prepare = invert . addAll . map parse . lines

handle :: String ->  IO ()
handle s = readFile s >>= print . (part1 &&& part2) . prepare

main = handle "input.txt"
