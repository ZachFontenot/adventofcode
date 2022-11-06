{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module GenDays where

import Data.String.Interpolate (i)
import qualified Data.Text.Lazy.IO as TIO
import System.Directory (doesFileExist)
import Data.Text.Lazy ( pack, Text )

makeDayFile :: String -> String  -> IO ()
makeDayFile year day = do
  let filename = year <> "/days/" <> day <> ".hs" :: FilePath
  fileExists <- doesFileExist filename
  if fileExists then do putStrLn "Day already exists"
    else TIO.writeFile filename $ fileStruct . pack $ day

makeAllFiles :: String -> IO ()
makeAllFiles year = makeFiles year days
  where
    days = map makeDayString [1 .. 25]

    makeDayString :: Integer -> String
    makeDayString num = if num < 10 then
      ("Day0" <>) . show $ num
      else ("Day" <>) . show $ num
      
    makeFiles year days = case days of
      [] -> putStrLn "Finis"
      (x:xs) -> do
        makeDayFile year x
        makeFiles year xs
  
fileStruct :: Text -> Text
fileStruct day = [i|{-\# LANGUAGE OverloadedStrings \#-}

module #{day} (solveDay) where

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
partB = error "Not implemented yet!"|]
