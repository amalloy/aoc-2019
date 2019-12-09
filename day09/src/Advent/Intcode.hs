module Advent.Intcode where

import Prelude hiding (pred, read)
import qualified Prelude as P

import Data.Bool (bool)
import Data.List.Split (splitOn)

import qualified Data.Map as M

type Address = Integer
type Value = Integer
type Memory = M.Map Value Value

data Opcode = Add | Mul
            | Input | Output
            | JumpIfTrue | JumpIfFalse
            | LessThan | Equals
            | RelativeBaseOffset
            | Halt deriving (Eq, Show)
data Operation = Operation { opcode :: Opcode
                           , identifier :: Integer
                           , numParams :: Integer
                           } deriving Show
operations :: M.Map Integer Operation
operations = M.fromList [(identifier op, op) | op <-
                            [ Operation Add 1 3
                            , Operation Mul 2 3
                            , Operation Input 3 1
                            , Operation Output 4 1
                            , Operation JumpIfTrue 5 2
                            , Operation JumpIfFalse 6 2
                            , Operation LessThan 7 3
                            , Operation Equals 8 3
                            , Operation RelativeBaseOffset 9 1
                            , Operation Halt 99 0
                            ]]

data ParameterMode = Position | Immediate | RelativeBase deriving Show
data Instruction = Instr Operation [(Value, ParameterMode)] deriving Show

type Program = [Integer]
data Computer = Computer { memory :: Memory
                         , ip :: Address
                         , inputs :: [Value]
                         , outputs :: [Value]
                         , running :: Bool
                         , relativeBase :: Address
                         } deriving Show

decodeNext :: Computer -> Either String Instruction
decodeNext Computer {memory = mem, ip = i} = do
  let code = M.findWithDefault 0 i mem
      instrNum = code `mod` 100
  operation <- case operations M.!? instrNum of
    Nothing -> Left $ "Unknown opcode " ++ show instrNum ++ " at offset " ++ show i
    Just op -> pure op
  let parameters = map (mem M.!) [(i + 1)..i + numParams operation]
      modes = map (intToMode . (`mod` 10)) . iterate (`div` 10) $ code `div` 100
  pure . Instr operation $ zip parameters modes
  where intToMode 0 = Position
        intToMode 1 = Immediate
        intToMode 2 = RelativeBase
        intToMode _ = error "Illegal parameter mode"

advanceIp :: Integer -> Computer -> Computer
advanceIp paramCount c = c { ip = ip c + paramCount }

execute :: Computer -> Instruction -> Either String Computer
execute c (Instr op params) = case opcode op of
  Add -> runBin (+)
  Mul -> runBin (*)
  LessThan -> runBin (comparison (<))
  Equals -> runBin (comparison (==))
  JumpIfTrue -> jump (/= 0)
  JumpIfFalse -> jump (== 0)
  Input -> advance $ case (inputs c, params) of
    ((i:is), [dst]) -> pure (updateMemory
                              (write [(mkAbsolute c dst, i)]) c) {inputs = is}
    actual -> Left $ "Invalid Input: " ++ show (actual, c)
  Output -> advance $ case params of
    [param] -> pure $ c {outputs = result : outputs c}
      where result = read param c
  RelativeBaseOffset -> advance $ case params of
    [param] -> pure $ c {relativeBase = relativeBase c + read param c}
  Halt -> pure $ c {running = False}
  where runBin f = advance $ case params of
          [a, b, dst] -> pure $ updateMemory (write [(mkAbsolute c dst, result)]) c
            where result = read a c `f` read b c

          actual -> Left $ "Invalid params: " ++ show actual
        comparison f x y = bool 0 1 $ x `f` y
        jump f = case params of
          [a, b] -> let newIp = if f $ read a c
                          then read b c
                          else 3 + ip c
                    in pure $ c {ip = newIp}
        advance = fmap (advanceIp (1 + numParams op))

mkAbsolute :: Computer -> (Address, ParameterMode) -> Address
mkAbsolute c (param, mode) = case mode of
  Position -> param
  RelativeBase -> relativeBase c + param
  _ -> error $ "unexpected input mode " ++ show mode

read :: (Integer, ParameterMode) -> Computer -> Value
read (x, Immediate) _ = x
read (a, Position) c = M.findWithDefault 0 a (memory c)
read (a, RelativeBase) c = M.findWithDefault 0 (a + relativeBase c) (memory c)

write :: [(Address, Value)] -> Memory -> Memory
write = flip $ foldr (uncurry M.insert)

updateMemory :: (Memory -> Memory) -> (Computer -> Computer)
updateMemory f c = c {memory = f $ memory c}

parseIntcodeProgram :: String -> Program
parseIntcodeProgram = map P.read . splitOn "," . head . lines

mkComputer :: Program -> [Value] -> Computer
mkComputer mem inputValues = Computer (M.fromList $ zip [0..] mem) 0 inputValues [] True 0

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

input :: Value -> Computer -> Computer
input x c = c {inputs = x : inputs c}
