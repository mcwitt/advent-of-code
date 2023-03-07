{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (asum, (<|>))
import Control.Arrow ((&&&), (>>>))
import Data.Functor (($>))
import Data.List (groupBy, tails)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Text.ParserCombinators.ReadP

main = interact solve

solve = read >>> (part1 &&& part2) >>> show

newtype Elves = Elves {unInput :: Set (V2 Int)} deriving (Eq)

instance Read Elves where
  readsPrec _ = readP_to_S input
    where
      input = Elves . Set.fromList . findIndices2 id <$> sepBy1 row (char '\n')
      row = many1 tile
      tile = char '.' $> False <|> char '#' $> True

instance Show Elves where
  show (Elves xs) =
    unlines
      [ [ if V2 r c `elem` xs then '#' else '.'
          | c <- [minimum cols .. maximum cols]
        ]
        | r <- [minimum rows .. maximum rows]
      ]
    where
      rows = map (\(V2 r _) -> r) $ Set.toList xs
      cols = map (\(V2 _ c) -> c) $ Set.toList xs

findIndices2 :: (a -> Bool) -> [[a]] -> [V2 Int]
findIndices2 p xss = [V2 r c | (r, xs) <- zip [1 ..] xss, (c, x) <- zip [1 ..] xs, p x]

data V2 a = V2 !a !a deriving (Show, Eq, Ord)

instance Num a => Num (V2 a) where
  V2 r1 c1 + V2 r2 c2 = V2 (r1 + r2) (c1 + c2)

data Cardinal = N | S | W | E deriving (Show)

normal :: Cardinal -> Int -> V2 Int
normal dir n = case dir of
  N -> V2 (-1) n
  S -> V2 1 (-n)
  W -> V2 (-n) (-1)
  E -> V2 n 1

tryMove :: Set (V2 Int) -> V2 Int -> Cardinal -> Maybe (V2 Int)
tryMove xs x dir
  | allClear = Just (x + normal dir 0)
  | otherwise = Nothing
  where
    allClear = all (`Set.notMember` xs) [x + normal dir n | n <- [-1, 0, 1]]

proposeMove :: Set (V2 Int) -> [Cardinal] -> V2 Int -> V2 Int
proposeMove xs dirs x
  | hasNeighbors = fromMaybe x $ asum $ map (tryMove xs x) dirs
  | otherwise = x
  where
    hasNeighbors = any (`Set.member` xs) [x + normal dir n | dir <- cardinals, n <- [0, 1]]

step :: Elves -> [Cardinal] -> Elves
step (Elves xs) dirs =
  Elves . Set.fromList $
    [ x
      | (dest, origins) <- Map.assocs destToOrigins,
        x <- case origins of
          [_] -> [dest]
          _ -> origins
    ]
  where
    destToOrigins = Map.fromListWith (++) proposed
    proposed = [(dest, [origin]) | origin <- Set.toList xs, let dest = proposeMove xs dirs origin]

windows :: Int -> [a] -> [[a]]
windows n = map (take n) . tails

countTiles :: Elves -> Int
countTiles (Elves xs) =
  (maximum rows - minimum rows + 1)
    * (maximum cols - minimum cols + 1)
    - length xs
  where
    rows = map (\(V2 r _) -> r) $ Set.toList xs
    cols = map (\(V2 _ c) -> c) $ Set.toList xs

part1 :: Elves -> Int
part1 elves = countTiles (steps elves !! 10)

steps elves = scanl step elves dirss

cardinals = [N, S, W, E]

dirss = windows 4 $ cycle cardinals

part2 :: Elves -> Int
part2 =
  fst
    . head
    . head
    . dropWhile (\case [_] -> True; _ -> False)
    . groupOn snd
    . zip [1 ..]
    . steps
  where
    groupOn p = groupBy (\a b -> p a == p b)
