module Main where

import Control.Arrow ((>>>))
import Data.List (nub, tails, transpose)

windows m = transpose . take m . tails

solve n =
  windows n
    >>> fmap (nub >>> length >>> (== n))
    >>> takeWhile not
    >>> length
    >>> (+ n)

part1 = solve 4

part2 = solve 14

main = do
  input <- readFile "input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
