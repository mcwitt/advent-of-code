{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative (asum, (<|>))
import Data.Array (Array, (!))
import Data.Array qualified as Array
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as Char
import Data.Ix qualified as Ix
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as Nel
import Data.Sequence (Seq (Empty, (:<|)), (|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Linear (V2 (V2))

data Input = Input {heights :: Array (V2 Int) Int, start :: V2 Int, end :: V2 Int} deriving (Show)

data Cell = Elevation Int | Start | End deriving (Eq, Show)

parse :: BS.ByteString -> Either String Input
parse = P.parseOnly input
  where
    input = mkInput <$> row `P.sepBy` "\n"
    row = P.many1 cell
    cell = elevation <|> start <|> end
    elevation = Elevation . Char.ord <$> P.satisfy Char.isAsciiLower
    start = Start <$ "S"
    end = End <$ "E"

mkInput :: [[Cell]] -> Input
mkInput rows@(col : _) =
  let nr = length rows
      nc = length col
      Just start = uncurry V2 <$> elemIndex2 Start rows
      Just end = uncurry V2 <$> elemIndex2 End rows
   in Input
        { heights = Array.listArray (V2 0 0, V2 (nr - 1) (nc - 1)) [elevation x | row <- rows, x <- row],
          start = start,
          end = end
        }
  where
    elevation = \case
      Elevation e -> e
      Start -> Char.ord 'a'
      End -> Char.ord 'z'

elemIndex2 :: Eq a => a -> [[a]] -> Maybe (Int, Int)
elemIndex2 e = asum . fmap (\(i, r) -> (i,) <$> List.elemIndex e r) . zip [0 ..]

neighbors2 s = fmap (+ s) offsets
  where
    offsets =
      [ V2 1 0,
        V2 0 1,
        V2 (-1) 0,
        V2 0 (-1)
      ]

validNeighbors heights x =
  filter
    ( \n ->
        Ix.inRange (Array.bounds heights) n
          && heights ! n - heights ! x <= 1
    )
    $ neighbors2 x

shortestPaths :: Ord a => (a -> [a]) -> [a] -> [NonEmpty a]
shortestPaths step = go Set.empty . Seq.fromList . fmap Nel.singleton
  where
    go _ Empty = []
    go seen (p@(x :| _) :<| q)
      | x `Set.member` seen = go seen q
      | otherwise = p : go (Set.insert x seen) (List.foldl' (|>) q [n Nel.<| p | n <- step x])

part1 Input {..} =
  let Just path = List.find ((== end) . Nel.head) $ shortestPaths (validNeighbors heights) [start]
   in length path - 1

part2 Input {..} =
  let startingPoints = [x | (x, h) <- Array.assocs heights, h == Char.ord 'a']
      Just path = List.find ((== end) . Nel.head) $ shortestPaths (validNeighbors heights) startingPoints
   in length path - 1

main = do
  rawInput <- BS.readFile "input.txt"
  input <- either fail pure $ parse rawInput
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
