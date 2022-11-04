module Main where

import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString.Char8 qualified as BS

type Input = ()

parse :: BS.ByteString -> Either String Input
parse = P.parseOnly input
  where
    input = undefined

part1 :: Input -> Int
part1 = undefined

part2 :: Input -> Int
part2 = undefined

main :: IO ()
main = do
  rawInput <- BS.readFile "input.txt"
  input <- either fail pure $ parse rawInput
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
