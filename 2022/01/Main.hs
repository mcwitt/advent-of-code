module Main where

import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString.Char8 qualified as BS
import Data.List qualified as List

type Input = [[Int]]

parse :: BS.ByteString -> Either String Input
parse = P.parseOnly input
  where
    input = group `P.sepBy1` P.count 2 P.endOfLine
    group = nat `P.sepBy1` P.endOfLine
    nat = read <$> P.many1 P.digit

part1 = maximum . fmap sum

part2 = sum . take 3 . reverse . List.sort . fmap sum

main = do
  rawInput <- BS.readFile "input.txt"
  input <- either fail pure $ parse rawInput
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
