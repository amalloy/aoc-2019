module Main where

import Advent.Intcode

import Control.Arrow ((&&&))

import qualified Data.Map as M

import Control.Monad (when)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import Debug.Trace

type Input = Program

type Position = (Int, Int)
data Direction = North | South | East | West deriving Show
data Turn = TurnLeft | TurnRight
data Color = White | Black deriving Show
data Robot = Robot { _direction :: Direction
                   , _position :: Position
                   } deriving Show

type Hull = M.Map Position Color
data Progress = Progress { _robot :: Robot
                         , _hull :: Hull
                         , _computer :: Computer
                         , _steps :: Int
                         , _visited :: [Position]
                         } deriving Show

turn :: Turn -> Direction -> Direction
turn TurnLeft d = case d of
  North -> West
  West -> South
  South -> East
  East -> North
turn TurnRight d = iterate (turn TurnLeft) d !! 3

intToTurn 0 = TurnLeft
intToTurn 1 = TurnRight

move :: Direction -> Position -> Position
move d (x, y) = case d of
  North -> (x, y + 1)
  East -> (x + 1, y)
  South -> (x, y - 1)
  West -> (x - 1, y)

colorAt :: Position -> Hull -> Color
colorAt = M.findWithDefault Black

colorToInt Black = 0
colorToInt White = 1

intToColor 0 = Black
intToColor 1 = White

type Stack a = StateT Progress (Except String) a

part1 :: Input -> Int
part1 p = either error (M.size . _hull) . runExcept . execStateT go $ initialState
  where initialState :: Progress
        initialState = Progress (Robot North (0, 0)) M.empty (mkComputer p [0]) 0 []
        go :: Stack ()
        go = do
          c <- gets _computer
          c' <- lift $ runUntilInputNeededOrDone c
          robot <- gets _robot
          hull <- gets _hull
          let position = _position robot
          case outputs c' of
            [dir, color] -> do
              let hull' = M.insert position (intToColor color) hull
                  dir' = turn (intToTurn dir) (_direction robot)
                  position' = (move dir' position)
                  robot' = Robot dir' position'
                  c'' = input (colorToInt (colorAt position' hull')) c'
              steps' <- gets (succ . _steps)
              visited' <- gets ((position :) . _visited)
              put $ Progress robot' hull' (c'' {outputs = []}) steps' visited'
            [] -> trace "No outputs" $ pure ()
            [o] -> trace ("Single output " ++ show o) $ pure ()
          if needsInput c'
            then go
            else do
              s <- get
              -- traceShow s $ pure ()
              pure ()



part2 :: Input -> ()
part2 i = ()

prepare :: String -> Input
prepare = parseIntcodeProgram

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
