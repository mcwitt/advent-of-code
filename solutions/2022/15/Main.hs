{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString.Char8 qualified as BS
import Data.Functor (($>))
import Data.List qualified as List
import Data.Maybe (catMaybes)
import Linear (V2 (V2))

type Input = [(Point, Point)]

type Point = V2 Int

parse :: BS.ByteString -> Either String Input
parse = P.parseOnly input
  where
    input = line `P.sepBy1` "\n"
    line = (,) <$> sensorPos <*> beaconPos
    sensorPos = V2 <$ "Sensor at x=" <*> int <* ", y=" <*> int
    beaconPos = V2 <$ ": closest beacon is at x=" <*> int <* ", y=" <*> int
    int = do
      s <- P.char '-' $> (-1) <|> pure 1
      (* s) . read <$> P.many1 P.digit

data Circle = C {center :: Point, radius :: Int} deriving (Show)

sensor pos beacon = C pos $ manhattan (pos - beacon)

manhattan (V2 x y) = abs x + abs y

inRange p C {..} = manhattan (p - center) <= radius

part1 input = length $ filter (\p -> any (inRange p) sensors && p `notElem` beacons) points
  where
    sensors = fmap (uncurry sensor) input
    beacons = fmap snd input
    rmax = maximum $ fmap radius sensors
    xs = fmap ((\(V2 x _) -> x) . center) sensors
    xmin = minimum xs
    xmax = maximum xs
    points = [V2 x 2000000 | x <- [xmin - rmax .. xmax + rmax]]

data DiagDir = L | R

data DiagSegment (d :: DiagDir) = S {x0 :: Int, y1 :: Int, y2 :: Int}

leftDiags :: Circle -> [DiagSegment 'L]
leftDiags (C (V2 x y) radius) =
  [ S (x + y - radius - 1) (y - radius) y,
    S (x + y + radius + 1) y (y + radius)
  ]

rightDiags :: Circle -> [DiagSegment 'R]
rightDiags (C (V2 x y) radius) =
  [ S (x - y + radius + 1) (y - radius) y,
    S (x - y - radius - 1) y (y + radius)
  ]

intersection :: DiagSegment 'L -> DiagSegment 'R -> Maybe Point
intersection l r
  | remainder == 0 && y1 l <= y && y <= y2 l && y1 r <= y && y <= y2 r = Just $ V2 (x0 r + y) y
  | otherwise = Nothing
  where
    (y, remainder) = (x0 l - x0 r) `divMod` 2

part2 input = xb * gridSize + yb
  where
    sensors = fmap (uncurry sensor) input
    points = List.nub $ catMaybes $ intersection <$> (sensors >>= leftDiags) <*> (sensors >>= rightDiags)
    gridSize = 4000000
    isValid (V2 x y) = 0 <= x && x <= gridSize && 0 <= y && y <= gridSize
    [V2 xb yb] = filter (\p -> isValid p && not (any (inRange p) sensors)) points

main = do
  rawInput <- BS.readFile "input.txt"
  input <- either fail pure $ parse rawInput
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
