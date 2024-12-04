{-# LANGUAGE OverloadedStrings #-}

module Day10 (solveDay) where

{- ORMOLU_DISABLE -}
import qualified SolveDay as S (solveDay, Day)
import Data.Attoparsec.Text
import Data.List (foldl', foldl1')
import Data.Text (Text)
import Data.Void
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List (elemIndex)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (elemIndices)
{- ORMOLU_ENABLE -}

solveDay :: S.Day
solveDay = S.solveDay inputParser partA partB

------------ PARSER ------------
tester :: Text
tester = "7-F7-\n.FJ|7\nSJLL7\n|F--J\nLJ.LJ"

inputParser :: Parser Input
inputParser = V.fromList <$> lineParser `sepBy` endOfLine

lineParser :: Parser (Vector Pipe)
lineParser = V.fromList <$> many' pipeParser

pipeParser :: Parser Pipe
pipeParser =
  choice
    [ H <$ "-",
      V <$ "|",
      F <$ "F",
      J <$ "J",
      L <$ "L",
      N <$ "7",
      S <$ "S",
      G <$ "."
    ]

------------ TYPES ------------
--               7
-- my input has -S|
--               7
data Pipe
  = H -- connects with H, F, J, L, N, S; left - right
  | V -- connects with V, F, J, L, N, S; up - down
  | F -- N right, J right down, L down
  | J -- F left up, N up, L left
  | L -- J right, F up, L right up
  | N -- F left, J down, L left down
  | S -- dunno the Shape
  | G -- ground
  deriving (Eq, Show)

type Point = (Int, Int)

type Input = Vector (Vector Pipe)

type OutputA = Void

type OutputB = Void

------------ Helpers -----------
inBounds :: Point -> Point -> Bool
inBounds size (x, y) =
  0 <= x && x < fst size
    && 0 <= y
    && y < snd size

getNeighbors :: Point -> Point -> [Point]
getNeighbors size (x, y) =
  filter
    (inBounds size)
    [ (x - 1, y - 1),
      (x - 1, y),
      (x - 1, y + 1),
      (x, y - 1),
      (x, y + 1),
      (x + 1, y - 1),
      (x + 1, y),
      (x + 1, y + 1)
    ]

getSize :: Vector (Vector a) -> Point
getSize vOfV = (V.length vOfV, V.length (vOfV V.! 0))

-- v `atPoint` pt
atPoint :: Vector (Vector a) -> Point -> a
atPoint input (x, y) = input V.! x V.! y

findS :: (Num b, Enum b) => Vector (Vector Pipe) -> (Int, b)
findS v = head $ [(x, y) | (y, line) <- zip [0 ..] v', x <- elemIndices S line]
  where
    v' = map V.toList . V.toList $ v

------------ PART A ------------

partA :: Input -> OutputA
partA = error "Not implemented yet!"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
