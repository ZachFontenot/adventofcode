{-# LANGUAGE OverloadedStrings #-}

module Day03 (solveDay) where

{- ORMOLU_DISABLE -}
import qualified SolveDay as S (solveDay, Day)
import Data.Attoparsec.Text
import Data.Char (isDigit)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Data.Vector (Vector)
import qualified Data.Vector as V
{- ORMOLU_ENABLE -}

solveDay :: S.Day
solveDay = S.solveDay inputParser partA partB

------------ NOTES -------------
{-
1. Find the beginning and end of any given number
2. Check neighbors of points to test if adjacent tiles
are symbols or .
3. Is there a data structure I could parse this into?

Things we need:
 Get Bounds
 Check Bounds
 Check neighbors
 if all neighbors then parse num else 0
-}
------------ PARSER -----------
test :: Text
test = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."

inputParser :: Parser Input
inputParser = takeText

-- This is sort of dumb, because I'm parsing into Text
-- and xforming the Text back into String
buildVector :: Text -> Vector (Vector Char)
buildVector =
  V.fromList . map V.fromList . lines . T.unpack

------------ TYPES ------------
data EnginePart = EnginePart Int [Point] Bool deriving (Eq, Show)

newtype Gear = Gear Point deriving (Eq, Show)

type Point = (Int, Int)

type Input = Text

type OutputA = Int

type OutputB = Int

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

getPoints :: Point -> [Point]
getPoints (rows, columns) = [(x, y) | x <- [0 .. rows - 1], y <- [0 .. columns - 1]]

atPoint :: Point -> Vector (Vector a) -> a
atPoint (x, y) input = input V.! x V.! y

isntFound :: Foldable t => t EnginePart -> Point -> Bool
isntFound engineParts point =
  all ((point `notElem`) . getEnginePoints) engineParts
  where
    getEnginePoints (EnginePart pnum pts _) = pts

getNumber :: Point -> Point -> Vector (Vector Char) -> EnginePart
getNumber size (x, y) input =
  EnginePart partNumber points False
  where
    currentPoint = atPoint (x, y) input
    (partNumber, points) = consumeNumber size (x, y + 1) input ([currentPoint], [(x, y)])

consumeNumber :: Point -> Point -> Vector (Vector Char) -> ([Char], [Point]) -> (Int, [Point])
consumeNumber size (x, y) input (numStr, points) =
  if inBounds size (x, y) && isDigit (atPoint (x, y) input)
    then consumeNumber size (x, y + 1) input (numStr <> [atPoint (x, y) input], (x, y) : points)
    else (read numStr :: Int, reverse points)

traverseInput :: Point -> Vector (Vector Char) -> [EnginePart] -> Point -> [EnginePart]
traverseInput size input engineParts point =
  if isDigit (atPoint point input) && isntFound engineParts point
    then getNumber size point input : engineParts
    else engineParts

getEngineParts :: Vector (Vector Char) -> [EnginePart]
getEngineParts input =
  foldl' (traverseInput totalSize input) [] allPositions
  where
    totalSize = getSize input
    allPositions = getPoints totalSize

isSymbol :: Vector (Vector Char) -> Point -> Bool
isSymbol input pt =
  atPoint pt input `notElem` ['0' .. '9'] <> "."

checkNeighbors :: Point -> Vector (Vector Char) -> Point -> Bool
checkNeighbors size input pt =
  any (isSymbol input) neighbors
  where
    neighbors = getNeighbors size pt

checkEngine :: Vector (Vector Char) -> EnginePart -> EnginePart
checkEngine input ep@(EnginePart pnum pts _) =
  if any (checkNeighbors boundaries input) pts
    then EnginePart pnum pts True
    else ep
  where
    boundaries = getSize input

isTouched :: EnginePart -> Bool
isTouched (EnginePart _ _ is) = is

getPart :: EnginePart -> Int
getPart (EnginePart part _ _) = part

getGears :: Vector (Vector Char) -> [Gear]
getGears input = foldl' checkIsGear [] pts
  where
    pts = getPoints (getSize input)
    checkIsGear gears point = if (==) '*' (atPoint point input) then Gear point : gears else gears

touchesGear :: Point -> [EnginePart] -> Gear -> [Int]
touchesGear size parts (Gear pt) =
  map getPart touchGears
  where
    touchGears = filter xPtGr parts
    xPtGr (EnginePart _ pts _) = any (`elem` pts) gearNeighbors
    gearNeighbors = getNeighbors size pt

------------ PART A ------------
partA :: Input -> OutputA
partA input = sum . map getPart . filter isTouched . map (checkEngine vec) . getEngineParts $ vec
  where
    vec = buildVector input

------------ PART B ------------
partB :: Input -> OutputB
partB input = sum $ map product $ filter ((==) 2 . length) $ map (touchesGear totalSize engines) gears
  where
    gears = getGears vec
    engines = getEngineParts vec
    vec = buildVector input
    totalSize = getSize vec
