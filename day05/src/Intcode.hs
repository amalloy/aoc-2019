module Intcode where

import Prelude hiding (read)

import qualified Data.Map as M
import qualified Data.Vector as V

type Address = Int
type Value = Int
type Memory = V.Vector Value

data Opcode = Add | Mul | Input | Output | Halt deriving Show
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
                            , Operation Halt 99 0
                            ]]

data ParameterMode = Position | Immediate deriving Show
data Instruction = Instr Operation [(Int, ParameterMode)] deriving Show

data Computer = Computer { memory :: Memory
                         , ip :: Address
                         , inputs :: [Value]
                         , outputs :: [Value]
                         , running :: Bool
                         } deriving Show

decodeNext :: Computer -> Instruction
decodeNext Computer {memory = mem, ip = i} =
  let code = mem V.! i
      instrNum = code `mod` 100
      operation = operations M.! instrNum
      parameters = V.toList $ V.slice (i + 1) (numParams operation) mem
      modes = map (intToMode . (`mod` 10)) . iterate (`div` 10) $ code `div` 100
  in Instr operation $ zip parameters modes
  where intToMode 0 = Position
        intToMode 1 = Immediate
        intToMode _ = error "Illegal parameter mode"

advanceIp :: Int -> Computer -> Computer
advanceIp paramCount c = c { ip = ip c + paramCount + 1 }

execute :: Computer -> Instruction -> Either String Computer
execute c (Instr op params) = advanceIp (numParams op) <$> case opcode op of
  Add -> runBin (+)
  Mul -> runBin (*)
  Input -> case (inputs c, params) of
    ((i:is), [(param, Position)]) -> pure (updateMemory
                                            (write [(param, i)]) c) {inputs = is}
    actual -> Left $ "Invalid Input: " ++ show actual
  Output -> case params of
    [param] -> pure $ c {outputs = result : outputs c}
      where result = read param $ memory c
  Halt -> pure $ c {running = False}
  where runBin f = case params of
          [a, b, (dst, Position)] -> pure $ updateMemory (write [(dst, result)] ) c
            where result = read a mem `f` read b mem
                  mem = memory c

read :: (Int, ParameterMode) -> Memory -> Value
read (x, Immediate) _ = x
read (a, Position) mem = mem V.! a

write :: [(Address, Value)] -> Memory -> Memory
write = flip (V.//)

updateMemory :: (Memory -> Memory) -> (Computer -> Computer)
updateMemory f c = c {memory = f $ memory c}

mkComputer :: [Int] -> [Value] -> Computer
mkComputer mem inputValues = Computer (V.fromList mem) 0 inputValues [] True

tick :: Computer -> Either String Computer
tick = execute <*> decodeNext

runToCompletion :: Computer -> Either String Computer
runToCompletion = head . dropWhile ok . iterate (>>= tick) . pure
  where ok (Left _) = False
        ok (Right c) = running c
