{-# LANGUAGE OverloadedStrings #-}

module Day01 where

import Control.Applicative (ZipList (ZipList, getZipList))
import Data.Attoparsec.Text.Lazy (Parser, decimal, endOfLine, sepBy)
import Data.List (tails)
import qualified SolveDay as S (Day, solveDay)

-- parseNums :: Integral a => T.Text -> a
-- parseNums input =
--   let parsed = parseOnly (signed decimal) input
--    in case parsed of
--         Left _ -> 0
--         Right num -> num

-- sumNums :: (Ord a, Num t) => [a] -> t -> t
-- sumNums list acc = case list of
--   [] -> acc
--   [_] -> acc
--   (x : y : xs) ->
--     if y > x
--       then sumNums (y : xs) (acc + 1)
--       else sumNums (y : xs) acc

-- THUMBS UP
solveDay :: S.Day
solveDay = S.solveDay inputParser partA partB

inputParser :: Parser [Int]
inputParser = decimal `sepBy` endOfLine

-- Part A --
consecPairs :: [a] -> [(a, a)]
consecPairs xs = zip xs (tail xs)

partA :: [Int] -> Int
partA = length . filter (uncurry (<)) . consecPairs

-- Part B --
windows :: Num a => Int -> [a] -> [[a]]
windows n = getZipList . traverse ZipList . take n . tails

partB :: [Int] -> Int
partB = partA . map sum . windows 3
