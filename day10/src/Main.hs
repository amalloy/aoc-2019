module Main where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.List (maximumBy, sortOn)
import Data.Ord (comparing)
import Data.Ratio

import qualified Data.Map as M
import qualified Data.Set as S

type Delta a = a
type Coord = (Rational, Rational)
type Input = M.Map Coord [Coord]

unit :: Coord -> Coord -> Delta Coord
unit (y1, x1) (y2, x2) | dx == 0 = (signum dy, 0)
                       | otherwise = ((dy / dx) * signum dx, signum dx)
  where dx = x2 - x1
        dy = y2 - y1

move :: Delta Coord -> Coord -> Coord
move (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

pointsObscuring :: Coord -> Coord -> [Coord]
pointsObscuring p q = let path = iterate (move (unit p q)) p
                          points = filter ((== 1) . denominator . fst) path
                      in takeWhile (/= q) $ tail points

canSee :: M.Map Coord a -> Coord -> Coord -> Bool
canSee m p q = not . any (`M.member` m) $ pointsObscuring p q

allVisibilities :: M.Map Coord a -> M.Map Coord [Coord]
allVisibilities m = M.mapWithKey visible m
  where visible p _ = let m' = M.delete p m
                      in filter (canSee m' p) (M.keys m')

part1 :: Input -> Int
part1 = maximum . map length . M.elems

angleFromYAxis :: Coord -> Double
angleFromYAxis (y, x) = rescale (atan2 (fromRational y) (fromRational x) + (pi / 2))
  where rescale theta | theta < 0 = theta + (2 * pi)
                      | otherwise = theta

part2 :: Input -> Rational
part2 m = let origin@(oy, ox) = fst . maximumBy (comparing (length . snd)) . M.assocs $ m
              m' = M.mapKeys (\(y, x) -> (y - oy, x - ox)) . M.delete origin $ m
              (y, x) = remove 200 m'
          in ((x + ox) * 100 + (y + oy))
  where remove n m | numCandidates < n = remove (n - numCandidates)
                                         (M.withoutKeys m (S.fromList candidates))
                   | otherwise = sortOn angleFromYAxis candidates !! (n - 1)
          where candidates = filter (canSee m (0, 0)) (M.keys m)
                numCandidates = length candidates


prepare :: String -> Input
prepare input = allVisibilities . M.fromList $ do
  (y, line) <- zip [0..] $ lines input
  (x, pt) <- zip [0..] line
  guard $ pt == '#'
  pure ((y, x), ())

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
