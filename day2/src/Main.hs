{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative (liftA2, liftA3)
import Control.Arrow ((&&&))
import qualified Data.IntMap as M

type Program = M.IntMap Int
type Input = Program
type Position = Int

data BinOp = Add | Multiply deriving Show
data Instr = Binary BinOp Position Position Position | End deriving Show

runBin :: BinOp -> (Int -> Int -> Int)
runBin Add = (+)
runBin Multiply = (*)

decode :: Program -> Position -> Maybe Instr
decode m pos = do
  code <- at pos
  case code of
    1 -> binOp Add
    2 -> binOp Multiply
    99 -> pure End
    _ -> Nothing
  where binOp op = liftA3 (Binary op) (at (pos + 1)) (at (pos + 2)) (at (pos + 3))
        at = (m M.!?)

eval :: Program -> Instr -> Maybe Program
eval m End = pure m
eval m (Binary op src1 src2 dst) = do
  result <- liftA2 (runBin op) (at src1) (at src2)
  pure $ M.insert dst result m
  where at = (m M.!?)

tick :: (Position, Program) -> Maybe (Position, Program)
tick (pos, p) = do
  instr <- decode p pos
  p' <- eval p instr
  pure (pos + 4, p')

run :: Int -> Int -> Program -> Program
run noun verb = go . (0,) . M.insert 1 noun . M.insert 2 verb
  where go p@(offset, program) = maybe program go $ tick p

part1 :: Input -> Maybe Int
part1 = M.lookup 0 . run 12 2

part2 :: Input -> [Int]
part2 p = do
  noun <- [0..99]
  verb <- [0..99]
  case M.lookup 0 $ run noun verb p of
    Just 19690720 -> [100 * noun + verb]
    _ -> []

prepare :: String -> Input
prepare s = M.fromList . zip [0..] . read $ "[" ++ init s ++ "]"

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
