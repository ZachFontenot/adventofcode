{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day02 (solveDay) where

{- ORMOLU_DISABLE -}
import qualified SolveDay as S (solveDay, Day)
import Data.Attoparsec.Text
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (foldl')
{- ORMOLU_ENABLE -}

solveDay :: S.Day
solveDay = S.solveDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = gameParser `sepBy` endOfLine

-- I just wanted to use some applicative syntax
pullParser :: Parser Pull
pullParser =
  (,) <$> decimal
    <* space
    <*> colorParser

roundParser :: Parser Round
roundParser =
  space
    *> pullParser `sepBy` string ", "

colorParser :: Parser ColorCube
colorParser =
  choice
    [ Red <$ string "red",
      Blue <$ string "blue",
      Green <$ string "green"
    ]

gameParser :: Parser Game
gameParser =
  Game
    <$ string "Game"
    <* space
    <*> decimal
    <* char ':'
    <*> roundParser `sepBy` char ';'

------------ TYPES ------------
data ColorCube = Red | Blue | Green
  deriving (Eq, Show)

type Pull = (Int, ColorCube)

type Round = [Pull]

data Game = Game
  { gameId :: Int,
    rounds :: [Round]
  }
  deriving (Eq, Show)

type Input = [Game]

type OutputA = Int

type OutputB = Int

------------ Helpers ------------
checkPull :: Pull -> Bool
checkPull pull = case pull of
  (num, Red) -> num <= 12 -- ARROWED
  (num, Blue) -> num <= 14
  (num, Green) -> num <= 13

checkGame :: Game -> Int
checkGame Game {gameId, rounds} =
  if all (all checkPull) rounds
    then gameId
    else 0

getBlue :: [a] -> a
getBlue = (!! 2)

getRed :: [a] -> a
getRed = (!! 0)

getGreen :: [a] -> a
getGreen = (!! 1)

getMaxRound :: [Int] -> Round -> [Int]
getMaxRound = foldl' maxRound

maxRound :: Ord a => [a] -> (a, ColorCube) -> [a]
maxRound mxs pull =
  case pull of
    (num, Red) -> [max r num, g, b]
    (num, Green) -> [r, max g num, b]
    (num, Blue) -> [r, g, max b num]
  where
    r = getRed mxs
    g = getGreen mxs
    b = getBlue mxs

minGame :: [Round] -> [Int]
minGame = foldl' getMaxRound [0, 0, 0]

------------ PART A  ------------
partA :: Input -> OutputA
partA = sum . map checkGame

------------ PART B  ------------
-- I don't know why I can't eta reduce here?
partB :: Input -> OutputB
partB input = sum $ map (product . minGame . rounds) input
