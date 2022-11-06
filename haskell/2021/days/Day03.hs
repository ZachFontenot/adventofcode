{-# LANGUAGE OverloadedStrings #-}

module Day03 (solveDay) where

{- ORMOLU_DISABLE -}
import qualified SolveDay as S (solveDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

binToDec :: Integral t => t -> t
binToDec n =
  if n == 0
    then n
    else n `mod` 10 + (2 * binToDec (n `div` 10))

-- (define (binary->decimal n)
--   (if (zero? n)
--       n
--       (+ (modulo n 10) (* 2 (binary->decimal (quotient n 10))))))

solveDay :: S.Day
solveDay = S.solveDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = error "Not implemented yet!"

------------ TYPES ------------
type Input = Void

type OutputA = Void

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA = error "Not implemented yet!"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
