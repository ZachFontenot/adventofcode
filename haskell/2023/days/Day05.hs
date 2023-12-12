{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day05 (solveDay) where

import Data.Attoparsec.Text as A
import Data.List (foldl')
import qualified Data.List as L
import qualified SolveDay as S (Day, solveDay)

solveDay :: S.Day
solveDay = S.solveDay inputParser partA partB

------------ PARSER ------------
inputParser :: A.Parser Input
inputParser = do
  (seeds, seedRanges) <- seedParser
  mappings <- mapParser `sepBy` many' endOfLine

  pure $
    SMap
      { seeds,
        seedRanges,
        mappings
      }

seedParser :: A.Parser ([Seed], [SeedRange])
seedParser = do
  _ <- string "seeds: "
  seedRanges <- seedRangeParser `sepBy` space
  let seeds = concat seedRanges
  pure (seeds, map (\sr -> SeedRange {seedStart = head sr, seedCount = sr !! 1}) seedRanges)

seedRangeParser :: Parser [Integer]
seedRangeParser = do
  start <- decimal
  _ <- space
  seedCount <- decimal
  pure [start, seedCount]

mapParser :: A.Parser [SeedMap]
mapParser = do
  _ <- manyTill anyChar endOfLine
  numLines <- rangeParser `sepBy` endOfLine
  let seedMap = makeSeedMap numLines
  pure seedMap

rangeParser :: A.Parser (Integer, Integer, Integer)
rangeParser = do
  dest <- decimal
  _ <- many' space
  source <- decimal
  _ <- many' space
  step <- decimal
  pure (dest, source, step)

makeSeedMap :: [(Integer, Integer, Integer)] -> [SeedMap]
makeSeedMap = L.sortBy (\s1 s2 -> compare (start s1) (start s2)) . map makeMap
  where
    makeMap (d, s, ln) = SeedMap {start = s, end = s + ln, diff = d - s}

------------ TYPES ------------
type Seed = Integer

data SeedMap = SeedMap
  { start :: Integer,
    end :: Integer,
    diff :: Integer
  }
  deriving (Eq, Show)

data SeedRange = SeedRange {seedStart :: Integer, seedCount :: Integer} deriving (Eq, Show)

data SeedToLocationMap = SMap
  { seeds :: [Integer],
    seedRanges :: [SeedRange],
    mappings :: [[SeedMap]]
  }
  deriving (Eq, Show)

type Input = SeedToLocationMap

type OutputA = Integer

type OutputB = Integer

------------ PART A ------------
getNextMapping :: Integer -> [SeedMap] -> Integer
getNextMapping num mapping =
  if null maybeNext
    then num
    else snd . head $ maybeNext
  where
    scans = scanl (checkNumInRange num) (False, 0) mapping
    maybeNext = dropWhile (\(bol, n) -> not bol) scans

followWholePath :: Integer -> [[SeedMap]] -> Integer
followWholePath = foldl' getNextMapping

checkNumInRange :: Integer -> p -> SeedMap -> (Bool, Integer)
checkNumInRange num _ (SeedMap {start, end, diff}) =
  (start <= num && num <= end, num + diff)

partA :: Input -> OutputA
partA input = minimum allNums
  where
    allNums = map (flip followWholePath $ mappings input) $ seeds $ input

------------ PART B ------------
applyMapping :: SeedRange -> [SeedMap] -> [SeedRange]
applyMapping seeds [] = [seeds]
applyMapping seeds@(SeedRange {seedStart, seedCount}) rules@(SeedMap {start, end, diff} : restRules)
  | seedStart + seedCount < start = applyMapping seeds restRules
  | seedStart > end = applyMapping seeds restRules
  | seedStart >= start && seedStart + seedCount <= end = [SeedRange {seedStart = seedStart + diff, seedCount}]
  | seedStart < start =
    SeedRange {seedStart, seedCount = start - seedStart} :
    applyMapping (SeedRange {seedStart = start, seedCount = seedCount - (start - seedStart)}) rules
  | otherwise =
    SeedRange {seedStart = seedStart + diff, seedCount = (end - start) - (seedStart - start)} :
    applyMapping (SeedRange {seedStart = end, seedCount = seedStart + seedCount - end}) restRules

partB :: Input -> OutputB
partB input = minimum . map seedStart $ foldl' mapSeedMaps seeds ms
  where
    ms = mappings input
    seeds = seedRanges input
    mapSeedMaps seedVals seedMaps = seedVals >>= (`applyMapping` seedMaps)
