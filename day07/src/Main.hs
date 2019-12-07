module Main where

import Advent.Intcode

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Monad (foldM)
import Data.Bool (bool)
import Data.List (permutations)

type Input = Program

runAmplifier :: Program -> Int -> Int -> Either String Int
runAmplifier program input phase = head . outputs <$> runToCompletion computer
  where computer = mkComputer program [phase, input]

part1 :: Input -> Either String Int
part1 p = fmap maximum . sequence $ do
  phases <- permutations [0..4]
  pure $ foldM (runAmplifier p) 0 phases

runFeedback :: Program -> [Int] -> Either String Int
runFeedback p phases = let amps = [mkComputer p [phase] | phase <- phases]
                       in runTillDone 0 amps
  where runTillDone seed progs = do
          (ps, carry) <- runOneStep seed progs
          case ps of
            [] -> pure carry
            _ -> runTillDone carry ps

runOneStep :: Value -> [Computer] -> Either String ([Computer], Value)
runOneStep seed [] = pure ([], seed)
runOneStep seed (p:ps) = do
  c <- runUntilCondition done (p {inputs = inputs p ++ [seed]})
  case outputs c of
    [] -> Left $ "No outputs in " ++ show c
    (result:_) -> do
      (afters, seed') <- runOneStep result ps
      pure (bool id (c:) (running c) afters, seed')
  where done = liftA2 (||) needsInput (not . running)

part2 :: Input -> Either String Int
part2 p = fmap maximum . sequence $ do
  phases <- permutations [5..9]
  pure $ runFeedback p phases

prepare :: String -> Input
prepare = parseIntcodeProgram

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
