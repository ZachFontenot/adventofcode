{-# LANGUAGE OverloadedStrings #-}

module Day01 (solveDay) where

import Data.Attoparsec.Text
import Data.Char (digitToInt, isDigit)
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Replace.Attoparsec.Text
import qualified SolveDay as S (Day, solveDay)

solveDay :: S.Day
solveDay = S.solveDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = takeText

------------ TYPES ------------
type Input = Text

type OutputA = Int

type OutputB = Int

----------- Helpers ------------
filterChars :: Text -> Text
filterChars = T.filter isDigit

parseNums :: Text -> Int
parseNums ch =
  fromDigits $ map digitToInt [beg, fin]
  where
    beg = T.head ch
    fin = T.last ch

getNums :: Text -> Int
getNums = parseNums . filterChars

fromDigits :: [Int] -> Int
fromDigits = foldl' (\num d -> 10 * num + d) 0

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . map getNums . T.lines

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map (getNums . updatedText . updatedText) . T.lines
  where
    nameParser =
      choice
        [ string "one",
          string "two",
          string "three",
          string "four",
          string "five",
          string "six",
          string "seven",
          string "eight",
          string "nine"
        ]

    mapNumWordTo = Map.fromList . zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] $ ["1e", "2o", "3e", "4", "5e", "6", "7n", "8t", "9e"]

    updatedText = streamEdit nameParser (mapNumWordTo Map.!)
