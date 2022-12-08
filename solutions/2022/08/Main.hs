module Main where

import Control.Monad (join)
import Data.Array (Array, (!))
import Data.Array qualified as Array
import Data.ByteString.Char8 qualified as BS
import Data.Char (digitToInt)
import Data.List qualified as List
import Linear (V2 (V2))

type Input = Array (V2 Int) Int

parse :: BS.ByteString -> Input
parse s =
  let xss = [[digitToInt c | c <- BS.unpack line] | line <- BS.lines s]
      n = length (head xss)
   in Array.listArray (V2 1 1, V2 n n) $ join xss

sightlines (V2 nr nc) (V2 r c) =
  [ [V2 r' c | r' <- reverse [1 .. r - 1]], -- up
    [V2 r c' | c' <- reverse [1 .. c - 1]], -- left
    [V2 r' c | r' <- [r + 1 .. nr]], -- down
    [V2 r c' | c' <- [c + 1 .. nc]] -- right
  ]

visible a (x, h) = any unblocked $ sightlines size x
  where
    (_, size) = Array.bounds a
    unblocked = all ((< h) . (a !))

part1 input = length . filter id . fmap (visible input) . Array.assocs $ input

scenicScore a (x, h) = product . fmap viewDist $ sightlines size x
  where
    (_, size) = Array.bounds a
    viewDist xs = maybe (length xs) (+ 1) $ List.findIndex (>= h) . fmap (a !) $ xs

part2 input = maximum . fmap (scenicScore input) . Array.assocs $ input

main = do
  rawInput <- BS.readFile "input.txt"
  let input = parse rawInput
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
