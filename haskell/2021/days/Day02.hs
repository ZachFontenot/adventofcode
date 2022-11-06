{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day02 where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text.Lazy
  ( Parser,
    decimal,
    endOfLine,
    sepBy,
    skipSpace,
    string,
  )
import Data.Functor (($>))
import Data.List (foldl')
import qualified SolveDay as S (Day, solveDay)

parseDirection :: Parser Direction
parseDirection =
  (string "forward" $> Forward)
    <|> (string "up" $> Up)
    <|> (string "down" $> Down)

instructionParser :: Parser [Instruction]
instructionParser =
  (Instruction <$> parseDirection <* skipSpace <*> decimal) `sepBy` endOfLine

data Direction = Forward | Up | Down
  deriving (Show, Eq)

data Instruction = Instruction {direction :: Direction, value :: Integer}
  deriving (Show, Eq)

dirCh :: (Integer, Integer) -> Instruction -> (Integer, Integer)
dirCh (h, d) Instruction {direction, value} =
  case direction of
    Forward -> (h + value, d)
    Up -> (h, d - value)
    Down -> (h, d + value)

twoCh ::
  (Integer, Integer, Integer) ->
  Instruction ->
  (Integer, Integer, Integer)
twoCh (h, d, a) Instruction {direction, value} =
  case direction of
    Forward -> (h + value, d + (a * value), a)
    Up -> (h, d, a - value)
    Down -> (h, d, a + value)

partA :: [Instruction] -> Integer
partA = uncurry (*) . foldl' dirCh (0, 0)

partB :: Foldable t => t Instruction -> Integer
partB = (\(h, d, _) -> h * d) . foldl' twoCh (0, 0, 0)

solveDay :: S.Day
solveDay = S.solveDay instructionParser partA partB
