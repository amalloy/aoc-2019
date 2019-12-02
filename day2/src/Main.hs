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

tick :: Program -> Position -> Maybe (Position, Program)
tick p pos = do
  instr <- decode p pos
  p' <- eval p instr
  pure (pos + 4, p')

part1 :: Input -> Maybe Int
part1 = go 0 . M.insert 1 12 . M.insert 2 2
  where go offset program = case tick program offset of
          Nothing -> M.lookup 0 program
          Just (offset', program') -> go offset' program'

part2 :: Input -> ()
part2 i = ()

prepare :: String -> Input
prepare s = M.fromList . zip [0..] . read $ "[" ++ init s ++ "]"

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
