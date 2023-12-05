{-# LANGUAGE OverloadedStrings #-}

module Day04 (solveDay) where

{- ORMOLU_DISABLE -}
import qualified SolveDay as S (solveDay, Day)
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Map as M
import Data.List (foldl')
{- ORMOLU_ENABLE -}

solveDay :: S.Day
solveDay = S.solveDay inputParser partA partB

test :: Text
test = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

------------ PARSER ------------
inputParser :: Parser Input
inputParser = cardParser `sepBy` endOfLine

cardParser :: Parser Card
cardParser = do
  _ <- string "Card"
  _ <- many' space
  cardId <- decimal
  _ <- char ':'
  _ <- many' space
  winningNums <- decimal `sepBy` many' space
  _ <- many' space
  _ <- char '|'
  _ <- many' space
  pickedNums <- decimal `sepBy` many' space

  pure $ Card cardId winningNums pickedNums

------------ TYPES ------------
data Card = Card Int [Int] [Int] deriving (Eq, Show)

data CardWithCount = CardWithCount
  { card :: Card,
    cardCount :: Int
  }
  deriving (Eq, Show)

type Input = [Card]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
checkWinningNumbers :: Card -> [Int]
checkWinningNumbers (Card _id win pulled) =
  filter (`elem` win) pulled

partA :: Input -> OutputA
partA = sum . map (\c -> 2 ^ (length c - 1)) . filter (not . null) . map checkWinningNumbers

------------ PART B ------------
checkAndMapToNumber :: Card -> Card
checkAndMapToNumber c@(Card cardId win _) =
  Card cardId win $ [length . checkWinningNumbers $ c]

makeCardMap :: [Card] -> M.Map Int CardWithCount
makeCardMap = foldl' insertCard M.empty
  where
    insertCard m c@(Card cid _ _) =
      M.insert cid CardWithCount {card = c, cardCount = 1} m

updateCardCount :: Int -> M.Map Int CardWithCount -> M.Map Int CardWithCount
updateCardCount =
  M.update updateCount
  where
    updateCount (CardWithCount {card = c, cardCount = ct}) =
      Just $ CardWithCount {card = c, cardCount = ct + 1}

updateCopies :: Int -> Int -> M.Map Int CardWithCount -> M.Map Int CardWithCount
updateCopies key ln m =
  foldr updateCardCount m keys
  where
    keys = map (+ key) [1 .. ln]

processWins :: M.Map Int CardWithCount -> Card -> M.Map Int CardWithCount
processWins m (Card cid _ matches) =
  if cardIds == 0
    then m
    else foldr (\_ cardMap -> updateCopies cid cardIds cardMap) m [1 .. nTimes]
  where
    cardIds = head matches
    nTimes = case M.lookup cid m of
      Just (CardWithCount {cardCount = ct}) -> ct
      Nothing -> 0

partB :: Input -> OutputB
partB input = M.foldr (\c s -> s + cardCount c) 0 $ foldl' processWins cardMap withWins
  where
    withWins = map checkAndMapToNumber input
    cardMap = makeCardMap withWins
