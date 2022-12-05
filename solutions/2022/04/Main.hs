module Main where

import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString.Char8 qualified as BS

type Input = [((Int, Int), (Int, Int))]

parse :: BS.ByteString -> Either String Input
parse = P.parseOnly input
  where
    input = pair `P.sepBy1` P.endOfLine
    pair = (,) <$> interval <* P.char ',' <*> interval
    interval = (,) <$> nat <* P.char '-' <*> nat
    nat = read <$> P.many1 P.digit

part1 = count (\(x, y) -> x `contains` y || y `contains` x)

count p = length . filter p

contains (a1, b1) (a2, b2) = a1 <= a2 && b2 <= b1

part2 = count $ uncurry overlap

overlap (a1, b1) (a2, b2) = a2 <= b1 && a1 <= b2

main = do
  rawInput <- BS.readFile "input.txt"
  input <- either fail pure $ parse rawInput
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
