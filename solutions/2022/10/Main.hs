{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString.Char8 qualified as BS

type Input = [Op]

data Op = NoOp | AddX Int deriving (Show)

parse :: BS.ByteString -> Either String Input
parse = P.parseOnly input
  where
    input = op `P.sepBy` "\n"
    op =
      AddX <$ "addx " <*> int
        <|> NoOp <$ "noop"
    int = fmap read $ (:) <$> (P.digit <|> P.char '-') <*> many P.digit

type Register = Int

trace :: [Op] -> [Register]
trace =
  scanl (+) 1
    . foldMap
      ( \case
          AddX dx -> [0, dx]
          NoOp -> [0]
      )

part1 input = let xs = trace input in sum [n * xs !! (n - 1) | n <- [20, 60 .. 220]]

part2 input = unlines $ fmap renderRow rows
  where
    renderRow = zipWith (\i x -> if abs (i - x) <= 1 then '#' else '.') [0 ..]
    rows = take 6 $ chunksOf 40 (trace input)

chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

main :: IO ()
main = do
  rawInput <- BS.readFile "input.txt"
  input <- either fail pure $ parse rawInput
  print $ trace input
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> "\n" <> part2 input
