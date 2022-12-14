{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString.Char8 qualified as BS
import Data.List qualified as List

type Input = [(Packet, Packet)]

data Packet = I Int | L [Packet] deriving (Eq, Show)

parse :: BS.ByteString -> Either String Input
parse = P.parseOnly input
  where
    input = P.sepBy1 pair "\n"
    pair = (,) <$> packet <* "\n" <*> packet <* "\n"
    packet = I <$> nat <|> L <$ "[" <*> P.sepBy packet "," <* "]"
    nat = read <$> P.many1 P.digit

instance Ord Packet where
  compare (I n) (I m) = compare n m
  compare (L ns) (L ms) = compare ns ms
  compare a@(L _) b@(I _) = compare a (L [b])
  compare a@(I _) b@(L _) = compare (L [a]) b

part1 = sum . fmap (+ 1) . List.findIndices (uncurry (<=))

part2 input = product $ fmap (+ 1) idxs
  where
    Just idxs = traverse (`List.elemIndex` sortedPackets) dividers
    sortedPackets = List.sort . (dividers <>) . (>>= \(a, b) -> [a, b]) $ input
    dividers = [L [L [I 2]], L [L [I 6]]]

main = do
  rawInput <- BS.readFile "input.txt"
  input <- either fail pure $ parse rawInput
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
