module Main where

{- ORMOLU_DISABLE -}

--- THE DAYS!!!
import Day01 ( solveDay )
import Day02 ( solveDay )
import Day03 ( solveDay )
import Day04 ( solveDay )
import Day05 ( solveDay )
import Day06 ( solveDay )
import Day07 ( solveDay )
import Day08 ( solveDay )
import Day09 ( solveDay )
import Day10 ( solveDay )
import Day11 ( solveDay )
import Day12 ( solveDay )
import Day13 ( solveDay )
import Day14 ( solveDay )
import Day15 ( solveDay )
import Day16 ( solveDay )
import Day17 ( solveDay )
import Day18 ( solveDay )
import Day19 ( solveDay )
import Day20 ( solveDay )
import Day21 ( solveDay )
import Day22 ( solveDay )
import Day23 ( solveDay )
import Day24 ( solveDay )
import Day25 ( solveDay )

import SolveDay ( Day, LogLevel(Quiet, Debug) )

{- ORMOLU_ENABLE -}

-- useful shiz
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment (getArgs)

days :: Map Int Day
days =
  Map.fromList . zip [1 ..] $
    [ Day01.solveDay,
      Day02.solveDay,
      Day03.solveDay,
      Day04.solveDay,
      Day05.solveDay,
      Day06.solveDay,
      Day07.solveDay,
      Day08.solveDay,
      Day09.solveDay,
      --Day10.solveDay,
      Day11.solveDay,
      Day12.solveDay,
      Day13.solveDay,
      Day14.solveDay,
      Day15.solveDay,
      Day16.solveDay,
      Day17.solveDay,
      Day18.solveDay,
      Day19.solveDay,
      Day20.solveDay,
      Day21.solveDay,
      Day22.solveDay,
      Day23.solveDay,
      Day24.solveDay,
      Day25.solveDay
    ]

main :: IO ()
main = do
  args <- getArgs
  let day = read . head $ args
  case days Map.!? day of
    Nothing -> putStrLn "Invalid day provided. 1 - 25"
    Just dayFunc -> do
      putStrLn "********STARTING********"
      _ <- dayFunc Quiet "2021" (show day)
      putStrLn "**********DONE**********"
