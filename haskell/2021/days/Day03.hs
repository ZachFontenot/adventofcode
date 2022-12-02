module Day03 (solveDay) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
  ( Parser,
    char,
    endOfLine,
    many1,
    sepBy,
  )
import Data.Functor (($>))
import Data.List (foldl', transpose)
import qualified SolveDay as S (Day, solveDay)

solveDay :: S.Day
solveDay = S.solveDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 (char '1' $> True <|> char '0' $> False) `sepBy` endOfLine

------------ TYPES ------------
type Input = [[Bool]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
countOnes :: [Bool] -> Ordering
countOnes bits =
  compare -- GT if more 1s than half, EQ if same, LT if 0 is more
    (length . filter id $ bits)
    (length . filter not $ bits)

partA :: Input -> OutputA
partA = uncurry (*) . foldl' buildBin (0, 0) . transpose
  where
    buildBin (gam, eps) bits =
      if countOnes bits == GT
        then (gam * 2 + 1, eps * 2)
        else (gam * 2, eps * 2 + 1)

------------ PART B ------------
data RatingType = Oxygen | CO2 deriving (Eq, Show)

findRating :: Input -> RatingType -> OutputB
findRating = findRating' 0
  where
    findRating' rating reports' ratingType =
      if length reports' == 1
        then foldl' (\acc bit -> 2 * acc + fromEnum bit) rating $ head reports'
        else
          let bit = getBit (head <$> reports') ratingType
           in findRating'
                (2 * rating + fromEnum bit)
                -- ["1111", "0000", "1010", "0101"] -> ["111", "010"] or [[True, True, True], [False, True, False]] etc
                (tail <$> filter ((== bit) . head) reports')
                ratingType

    getBit :: [Bool] -> RatingType -> Bool
    getBit nums rtype =
      (countOnes nums, rtype) `elem` [(GT, Oxygen), (LT, CO2), (EQ, Oxygen)]

partB :: Input -> OutputB
partB bins = findRating bins Oxygen * findRating bins CO2

-- Thanks Sam Coy

-- (tail <$> filter ((== bigBit) . head) listOfBits) the filter

-- (list for oxygen, list for co2)
-- filter most common, filter least common
-- foldl' (filter (countOnes . (>= EQ))
