{-# LANGUAGE OverloadedStrings #-}

module Day25 (solveDay) where

{- ORMOLU_DISABLE -}
import qualified SolveDay as S (solveDay, Day)
import Data.Attoparsec.Text as A
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
{- ORMOLU_ENABLE -}

solveDay :: S.Day
solveDay = S.solveDay inputParser partA partB

------------ PARSER ------------
tester :: Text
tester = "jqt: rhn xhk nvd\nrsh: frs pzl lsr\nxhk: hfx\ncmg: qnr nvd lhk bvb\nrhn: xhk bvb hfx\nbvb: xhk hfx\npzl: lsr hfx nvd\nqnr: nvd\nntq: jqt hfx bvb xhk\nnvd: lhk\nlsr: lhk\nrzs: qnr cmg lsr rsh\nfrs: qnr lhk lsr"

inputParser :: Parser Input
inputParser = do
  wireMaps <- wireParser `sepBy` endOfLine
  pure (fst . head $ wireMaps, makeWireDiagram wireMaps)

wireParser :: Parser (Text, [Text])
wireParser = do
  wire <- A.take 3
  _ <- ": "
  wireSpace <- A.takeWhile (/= '\n')
  let wires = T.split (== ' ') wireSpace
  pure (wire, wires)

makeWireStep :: Map Text [Text] -> (Text, [Text]) -> Map Text [Text]
makeWireStep m (wire, wires) =
  foldl'
    (\acc rwire -> M.insertWith (<>) rwire [wire] acc)
    initialMap
    wires
  where
    initialMap = M.insertWith (<>) wire wires m

makeWireDiagram :: [(Text, [Text])] -> Map Text [Text]
makeWireDiagram = foldl' makeWireStep M.empty

------------ TYPES ------------
type Input = (Text, Map Text [Text])

type OutputA = Int

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA input = 1

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
