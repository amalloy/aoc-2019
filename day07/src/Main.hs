module Main where

import Advent.Intcode

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Monad (foldM, when)
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)
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
                       in runExcept $ runTillDone 0 amps
  where runTillDone seed progs = do
          (carry, ps) <- runWriterT $ runOneStep seed progs
          case ps of
            [] -> pure carry
            _ -> runTillDone carry ps

runOneStep :: Value -> [Computer] -> WriterT [Computer] (Except String) Value
runOneStep = foldM runOneComputer
  where runOneComputer seed c = do
          c <- lift . except $ runUntilCondition done (c {inputs = inputs c ++ [seed]})
          case outputs c of
            [] -> lift . throwE $ "No outputs in " ++ show c
            (result:_) -> do
              when (running c) $ tell [c]
              pure result
        done = liftA2 (||) needsInput (not . running)

part2 :: Input -> Either String Int
part2 p = fmap maximum . sequence $ do
  phases <- permutations [5..9]
  pure $ runFeedback p phases

prepare :: String -> Input
prepare = parseIntcodeProgram

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
