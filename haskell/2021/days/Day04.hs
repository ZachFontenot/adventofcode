{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day04 (solveDay) where

{- ORMOLU_DISABLE -}
import qualified SolveDay as S (solveDay, Day)
import Data.Attoparsec.Text
    ( Parser,
      char,
      endOfLine,
      count,
      sepBy,
      sepBy1,
      decimal,
      skipSpace )
import Data.List (scanl', transpose, find)
import Data.Maybe (fromJust)
{- ORMOLU_ENABLE -}

solveDay :: S.Day
solveDay = S.solveDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,) <$> (calledNumbers <* count 2 endOfLine) <*> bingoCards
  where
    calledNumbers = decimal `sepBy` char ','
    bingoCards =
      count 5 (count 5 $ skipSpace *> ((,False) <$> decimal))
        `sepBy1` count 2 endOfLine

------------ TYPES ------------
type Input = ([Int], [BingoCard])

type OutputA = Int

type OutputB = Int

type BingoCard = [[(Int, Bool)]]

------------ PART A ------------
markCard :: Int -> BingoCard -> BingoCard
markCard num =
  fmap (fmap (\square -> if square == (num, False) then (num, True) else square))

checkCard :: BingoCard -> Bool
checkCard card =
  any (and . fmap snd) card
    || any (and . fmap snd) (transpose card)    

getScore :: Int -> BingoCard -> Int
getScore num card =
  (*) num $
    sum . fmap fst . filter (not . snd) . concat $ card

runBingoGame :: Input -> [(Int, [BingoCard])]
runBingoGame (nums, bingoCards) =
  zip nums . tail $
    scanl' (\cards number -> markCard number <$> filter (not . checkCard) cards) bingoCards nums

playBingo :: ((BingoCard -> Bool) -> [BingoCard] -> Bool) -> Input -> Int
playBingo condition input = sum . fmap (getScore lastNumber) . filter checkCard $ cards
  where
    (lastNumber, cards) = fromJust . find (condition checkCard . snd) . runBingoGame $ input

partA :: Input -> OutputA
partA = playBingo any

------------ PART B ------------
partB :: Input -> OutputB
partB = playBingo all

-- Thanks Sam Coy again. I promise I'm on my own from now on.
