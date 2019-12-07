module Advent.Intcode where

import Prelude hiding (read)
import qualified Prelude as P

import Data.Bool (bool)
import Data.List.Split (splitOn)

import qualified Data.Map as M
import qualified Data.Vector as V

type Address = Int
type Value = Int
type Memory = V.Vector Value

data Opcode = Add | Mul
            | Input | Output
            | JumpIfTrue | JumpIfFalse
            | LessThan | Equals
            | Halt deriving (Eq, Show)
data Operation = Operation { opcode :: Opcode
                           , identifier :: Int
                           , numParams :: Int
                           } deriving Show
operations :: M.Map Int Operation
operations = M.fromList [(identifier op, op) | op <-
                            [ Operation Add 1 3
                            , Operation Mul 2 3
                            , Operation Input 3 1
                            , Operation Output 4 1
                            , Operation JumpIfTrue 5 2
                            , Operation JumpIfFalse 6 2
                            , Operation LessThan 7 3
                            , Operation Equals 8 3
                            , Operation Halt 99 0
                            ]]

data ParameterMode = Position | Immediate deriving Show
data Instruction = Instr Operation [(Int, ParameterMode)] deriving Show

type Program = [Int]
data Computer = Computer { memory :: Memory
                         , ip :: Address
                         , inputs :: [Value]
                         , outputs :: [Value]
                         , running :: Bool
                         } deriving Show

decodeNext :: Computer -> Either String Instruction
decodeNext Computer {memory = mem, ip = i} = do
  let code = mem V.! i
      instrNum = code `mod` 100
  operation <- case operations M.!? instrNum of
    Nothing -> Left $ "Unknown opcode " ++ show instrNum ++ " at offset " ++ show i
    Just op -> pure op
  let parameters = V.toList $ V.slice (i + 1) (numParams operation) mem
      modes = map (intToMode . (`mod` 10)) . iterate (`div` 10) $ code `div` 100
  pure . Instr operation $ zip parameters modes
  where intToMode 0 = Position
        intToMode 1 = Immediate
        intToMode _ = error "Illegal parameter mode"

advanceIp :: Int -> Computer -> Computer
advanceIp paramCount c = c { ip = ip c + paramCount }

execute :: Computer -> Instruction -> Either String Computer
execute c (Instr op params) = case opcode op of
  Add -> runBin (+)
  Mul -> runBin (*)
  LessThan -> runBin (comparison (<))
  Equals -> runBin (comparison (==))
  JumpIfTrue -> jump (/= 0)
  JumpIfFalse -> jump (== 0)
  Input -> advanceIp 2 <$> case (inputs c, params) of
    ((i:is), [(param, Position)]) -> pure (updateMemory
                                            (write [(param, i)]) c) {inputs = is}
    actual -> Left $ "Invalid Input: " ++ show actual
  Output -> advanceIp 2 <$> case params of
    [param] -> pure $ c {outputs = result : outputs c}
      where result = read param mem
  Halt -> pure $ c {running = False}
  where runBin f = advanceIp 4 <$> case params of
          [a, b, (dst, Position)] -> pure $ updateMemory (write [(dst, result)] ) c
            where result = read a mem `f` read b mem
        comparison f x y = bool 0 1 $ x `f` y
        jump f = case params of
          [a, b] -> let newIp = if f $ read a mem
                          then read b mem
                          else 3 + ip c
                    in pure $ c {ip = newIp}
        mem = memory c

read :: (Int, ParameterMode) -> Memory -> Value
read (x, Immediate) _ = x
read (a, Position) mem = mem V.! a

write :: [(Address, Value)] -> Memory -> Memory
write = flip (V.//)

updateMemory :: (Memory -> Memory) -> (Computer -> Computer)
updateMemory f c = c {memory = f $ memory c}

parseIntcodeProgram :: String -> Program
parseIntcodeProgram = map P.read . splitOn "," . head . lines

mkComputer :: Program -> [Value] -> Computer
mkComputer mem inputValues = Computer (V.fromList mem) 0 inputValues [] True

tick :: Computer -> Either String Computer
tick c = do
  instr <- decodeNext c
  execute c instr

needsInput :: Computer -> Bool
needsInput c = null (inputs c) && case decodeNext c of
  Right (Instr (Operation Input _ _) _) -> True
  _ -> False

runUntilCondition :: (Computer -> Bool) -> Computer -> Either String Computer
runUntilCondition pred = head . dropWhile (not . done) . iterate (>>= tick) . pure
  where done (Left _) = True
        done (Right c) = pred c

runToCompletion :: Computer -> Either String Computer
runToCompletion = runUntilCondition (not . running)
