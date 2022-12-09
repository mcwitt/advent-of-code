{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString.Char8 qualified as BS
import Data.List (nub)
import Linear (V2 (V2))

data Dir = R | D | L | U deriving (Show)

data Move = Move Dir Int deriving (Show)

type Input = [Move]

parse :: BS.ByteString -> Either String Input
parse = P.parseOnly input
  where
    input = move `P.sepBy` "\n"
    move = Move <$> dir <* " " <*> nat
    dir =
      R <$ "R"
        <|> D <$ "D"
        <|> L <$ "L"
        <|> U <$ "U"
    nat = read <$> P.many1 P.digit

offset = \case
  R -> V2 1 0
  D -> V2 0 (-1)
  L -> V2 (-1) 0
  U -> V2 0 1

offsets (Move dir steps) = replicate steps $ offset dir

allOffsets = (>>= offsets)

pathFrom = scanl (+)

moveTail tailPos headPos =
  let d@(V2 dx dy) = headPos - tailPos
   in if abs dx > 1 || abs dy > 1 then tailPos + signum d else tailPos

headPath = pathFrom (V2 0 0) . allOffsets

tailPath = scanl1 moveTail

numVisited = length . nub

part1 = numVisited . tailPath . headPath

part2 = numVisited . (!! 9) . iterate tailPath . headPath

main = do
  rawInput <- BS.readFile "input.txt"
  input <- either fail pure $ parse rawInput
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
