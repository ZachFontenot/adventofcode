module Day10 where

import Data.List (sort)
import Data.Map (Map, fromList, lookup, notMember, (!))
import Data.Maybe (catMaybes)
import Data.Monoid (Sum (Sum, getSum))
import Prelude hiding (lookup)

{-(c : cs) (p : ps) if ( c = '(' and p = ')')
  continue with cs and ps
-}

tester :: String
tester = "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"

test :: [String]
test = lines tester

-- bracketPairs ! '{' -> '}' exception if not found
-- lookup K bracketPairs -> Maybe V
bracketPairs :: Map Char Char
bracketPairs =
  fromList [(')', '('), (']', '['), ('}', '{'), ('>', '<')]

bracketVals :: Map Char Int
bracketVals =
  fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

matchPairs :: String -> [Char] -> (Maybe Char, Maybe String)
matchPairs [] [] = (Nothing, Nothing)
matchPairs [] ps@(_ : _) = (Nothing, Just ps)
matchPairs (c : cs) []
  | notMember c bracketPairs = matchPairs cs (c : [])
  | otherwise = (Just c, Nothing)
matchPairs (c : cs) (p : ps)
  | notMember c bracketPairs = matchPairs cs (c : p : ps)
  | otherwise = case (lookup c bracketPairs) of
    Nothing -> (Just c, Nothing) -- Not sure about this case?
    Just val | val == p -> matchPairs cs ps
    _ -> (Just c, Nothing)

convertCharToVal :: Maybe Char -> Maybe Int
convertCharToVal Nothing = Nothing
convertCharToVal (Just ch) = lookup ch bracketVals

getAllSum :: [Maybe Int] -> Int
getAllSum ls = getSum $ foldMap (maybe mempty Sum) ls

solve1 :: [String] -> Int
solve1 ls = getAllSum $ fmap (convertCharToVal . fst) $ getStrings ls

-- We don't actually map to the missing sides, because we don't actually need to
-- Reversing the list is also not necessary since + and * are associative
twoVals :: Map Char Int
twoVals = fromList [('(', 1), ('[', 2), ('{', 3), ('<', 4)]

toNum :: Char -> Int
toNum c = twoVals ! c

mapToNum :: String -> [Int]
mapToNum ss = fmap toNum ss

partTwoOp :: String -> Int
partTwoOp ss = Prelude.foldl (\x y -> (x * 5) + y) 0 (mapToNum ss)

-- This function is a little gnarly, I'd like to maybe change it to use Do notation
-- If I can?
solve2 :: [String] -> [Int]
solve2 ls = (middle . sort) $ fmap partTwoOp $ catMaybes $ fmap snd $ getStrings ls

getStrings :: Functor f => f String -> f (Maybe Char, Maybe String)
getStrings = ((flip matchPairs []) <$>)

-- Thanks SO
middle :: [a] -> [a]
middle l@(_ : _ : _ : _) = middle $ tail $ init l
middle l = l

solveDay :: IO ()
solveDay = do
  input <- readFile "2021/days/day10.txt"
  let brackets = lines input
  let one = solve1 brackets
  let two = head $ solve2 brackets
  print one
  print two
