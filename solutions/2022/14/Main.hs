{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString.Char8 qualified as BS
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Linear (V2 (V2))

type Input = [Path]

type Path = [V2 Int]

parse :: BS.ByteString -> Either String Input
parse = P.parseOnly input
  where
    input = path `P.sepBy` "\n"
    path = point `P.sepBy1` " -> "
    point = V2 <$> nat <* "," <*> nat
    nat = read <$> P.many1 P.digit

pathPoints xs = mconcat . zipWith segmentPoints xs $ tail xs

segmentPoints (V2 x1 y1) (V2 x2 y2)
  | x1 == x2 = [V2 x1 y | y <- [min y1 y2 .. max y1 y2]]
  | y1 == y2 = [V2 x y1 | x <- [min x1 x2 .. max x1 x2]]

dropSand :: (V2 Int -> Bool) -> (V2 Int -> Bool) -> V2 Int -> Maybe (V2 Int)
dropSand escaped available = go
  where
    go p@(V2 _ y)
      | escaped p = Just p
      | available p =
          go (below p)
            <|> go (below (left p))
            <|> go (below (right p))
            <|> Just p
      | otherwise = Nothing

    below = (+ V2 0 1)
    left = (+ V2 (-1) 0)
    right = (+ V2 1 0)

next ymax dropFrom blocked = do
  p <- dropSand (\(V2 _ y) -> y > ymax) (`Set.notMember` blocked) dropFrom
  pure (p, Set.insert p blocked)

part1 paths =
  let points = [point | path <- paths, point <- pathPoints path]
      ymax = maximum $ fmap (\(V2 _ y) -> y) points
      blocked = Set.fromList points
   in fromJust $ List.findIndex (\(V2 _ y) -> y > ymax) $ List.unfoldr (next ymax (V2 500 0)) blocked

next2 yfloor dropFrom blocked = do
  p <- dropSand (const False) (\p@(V2 _ y) -> p `Set.notMember` blocked && y < yfloor) dropFrom
  pure (p, Set.insert p blocked)

part2 paths =
  let points = [point | path <- paths, point <- pathPoints path]
      ymax = maximum $ fmap (\(V2 _ y) -> y) points
      blocked = Set.fromList points
   in length $ List.unfoldr (next2 (ymax + 2) (V2 500 0)) blocked

main :: IO ()
main = do
  rawInput <- BS.readFile "input.txt"
  input <- either fail pure $ parse rawInput
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
