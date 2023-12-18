{-# LANGUAGE OverloadedStrings #-}

module Day06 (solveDay) where

import Data.Attoparsec.Text
import Data.List (foldl')
import qualified SolveDay as S (Day, solveDay)

solveDay :: S.Day
solveDay = S.solveDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  _ <- string "Time:"
  _ <- many' space
  times <- decimal `sepBy` many' space
  _ <- endOfLine
  _ <- string "Distance:"
  _ <- many' space
  distances <- decimal `sepBy` many' space
  _ <- endOfLine
  pure (times, distances)

------------ TYPES ------------
type Input = ([Int], [Int])

type OutputA = Int

type OutputB = Int

------------ PART A ------------
generateTimes :: Int -> [Int]
generateTimes t = foldl' (\acc num -> (t - num) * num : acc) [] [1 .. t]

filterAndGenerate :: (Int, Int) -> [Int]
filterAndGenerate (t, d) = filter (d <=) $ generateTimes t

partA :: Input -> OutputA
partA input = product $ map (length . filterAndGenerate) timesAndDist
  where
    timesAndDist = uncurry zip input

------------ PART B ------------
partB :: Input -> OutputB
partB input = length $ filterAndGenerate (time, distance)
  where
    getSingleNum :: [Int] -> Int
    getSingleNum nums = read $ concatMap show nums
    time = getSingleNum (fst input)
    distance = getSingleNum (snd input)
