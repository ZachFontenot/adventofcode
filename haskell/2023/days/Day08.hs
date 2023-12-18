{-# LANGUAGE OverloadedStrings #-}

module Day08 (solveDay) where

import Data.Attoparsec.Text
import Data.List (foldl', foldl1')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text, index)
import qualified Data.Text as T
import qualified SolveDay as S (Day, solveDay)
import Prelude hiding (take, takeWhile)

solveDay :: S.Day
solveDay = S.solveDay inputParser partA partB

------------ PARSER ------------
tester :: Text
tester = "RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)"

inputParser :: Parser Input
inputParser =
  (,)
    <$> takeWhile (/= '\n')
    <* many' endOfLine
    <*> mappingParser `sepBy` endOfLine

mappingParser :: Parser (Text, Text, Text)
mappingParser =
  (,,)
    <$> take 3
    <* " = ("
    <*> take 3
    <* ", "
    <*> take 3
    <* ")"

------------ TYPES ------------
-- data DesertMap = DesertMap
--   { instructions :: Text,
--     mappings :: Map Text (Text, Text)
--   }
--   deriving (Eq, Show)

type CamelMap = Map Text (Text, Text)

type Input = (Text, [(Text, Text, Text)])

type OutputA = Integer

type OutputB = Integer

------------ PART A ------------
left :: CamelMap -> Text -> Text
left cm key = fst $ cm M.! key

right :: CamelMap -> Text -> Text
right cm key = snd $ cm M.! key

buildMap :: [(Text, Text, Text)] -> CamelMap
buildMap = foldl' (\m (key, lft, rght) -> M.insert key (lft, rght) m) M.empty

followMap :: Text -> CamelMap -> Text -> Text -> Integer
followMap instructions cm check =
  followMap' 0 0
  where
    ln = T.length instructions
    followMap' i steps value =
      if check `T.isSuffixOf` value
        then steps
        else followMap' ((i + 1) `mod` ln) (steps + 1) nextValue
      where
        struct = index instructions i
        nextValue =
          if struct == 'L'
            then left cm value
            else right cm value

partA :: Input -> OutputA
partA input = followMap ints camelMap "ZZZ" "AAA"
  where
    ints = fst input
    camelMap = buildMap $ snd input

------------ PART B ------------

partB :: Input -> OutputB
partB input =
  foldl1' lcm $
    map (followMap ints camelMap "Z") startingPositions
  where
    ints = fst input
    camelMap = buildMap $ snd input
    startingPositions = map (\(k, _, _) -> k) $ filter (\(k, _, _) -> T.last k == 'A') $ snd input
