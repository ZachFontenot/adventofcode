{-# LANGUAGE OverloadedStrings #-}

module Day13 (solveDay) where

{- ORMOLU_DISABLE -}
import qualified SolveDay as S (solveDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

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