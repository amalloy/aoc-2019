module Main where

import Control.Arrow ((&&&))

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

input :: Parser [Mass]
input = planet `sepEndBy` newline

planet :: Parser Mass
planet = char '<' *> values <* char '>'
  where values = mkPlanet <$> dimension `sepBy` (string ", ")
        dimension = anyChar *> char '=' *> (read <$> many1 (digit <|> char '-'))
        mkPlanet = (`Mass` [0, 0, 0])

data Mass = Mass { pos :: [Int]
                 , v :: [Int]
                 } deriving (Show, Eq)

type Input = [Mass]

energy :: Mass -> Int
energy = (*) <$> total pos <*> total v
  where total f = sum . map abs . f

(<+>) :: [Int] -> [Int] -> [Int]
(<+>) = zipWith (+)

travel :: Mass -> Mass
travel (Mass pos v) = Mass (pos <+> v) v

gravity :: [Mass] -> Mass -> Mass
gravity system (Mass p v) = Mass p dv
  where dv = foldl (<+>) v $ do
          planet <- system
          pure $ zipWith pull (pos planet) p
        pull = (signum .) . (-)

tick :: [Mass] -> [Mass]
tick ms = map (travel . gravity ms) ms

part1 :: Input -> Int
part1 = sum . map energy . (!! 1000) . iterate tick

part2 :: Input -> ()
part2 i = ()

filename :: String
filename = "input.txt"

prepare :: String -> Input
prepare = either (error . show) id . parse input filename

main :: IO ()
main = readFile filename >>= print . (part1 &&& part2) . prepare
