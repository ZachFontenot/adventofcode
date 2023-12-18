{-# LANGUAGE OverloadedStrings #-}

module Day09 (solveDay) where

{- ORMOLU_DISABLE -}
import qualified SolveDay as S (solveDay, Day)
import Data.Attoparsec.Text
import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as P
import Data.List (foldl', foldl1')
import Prelude hiding (takeWhile)
import Data.Text (Text)
import Data.Void
{- ORMOLU_ENABLE -}

solveDay :: S.Day
solveDay = S.solveDay inputParser partA partB

------------ PARSER ------------
tester :: Text
tester = "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"

inputParser :: Parser Input
inputParser =
  numParser `sepBy` endOfLine

numParser :: Parser [Int]
numParser =
  (pPos <|> pNeg) `sepBy` char ' '
  where
    pPos = decimal
    pNeg = do
      _ <- char '-'
      n <- decimal
      return (-n)

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
getDiffs :: [Int] -> [Int]
getDiffs = tail . reverse . fst . foldl' (\(diffs, prev) num -> (num - prev : diffs, num)) ([], 0)

getAllDiffs :: [Int] -> [[Int]]
getAllDiffs nums =
  getAllDiffs' nums [nums]
  where
    getAllDiffs' nums' acc =
      if all (== 0) next
        then acc
        else getAllDiffs' next (next : acc)
      where
        next = getDiffs nums'

project :: [Int] -> [Int] -> [Int]
project toProject nextDiffs =
  nextDiffs <> [last toProject + last nextDiffs]

projectHead :: [Int] -> [Int] -> [Int]
projectHead toProject nextDiffs =
  head nextDiffs - head toProject : nextDiffs

partA :: Input -> OutputA
partA input = sum $ map (last . foldl1' project . getAllDiffs) $ filter (not . null) input

------------ PART B ------------
partB :: Input -> OutputB
partB input = sum $ map (head . foldl1' projectHead . getAllDiffs) $ filter (not . null) input
