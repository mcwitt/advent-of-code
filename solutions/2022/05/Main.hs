{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString.Char8 qualified as BS
import Data.Char as Char
import Data.Functor (($>))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.Maybe qualified as Maybe

type Input = (Stacks, [Instruction])

type Stacks = IntMap [Crate]

type Crate = Char

data Instruction = Instruction {number :: Int, from :: Int, to :: Int} deriving (Show)

parse :: BS.ByteString -> Either String Input
parse = P.parseOnly input
  where
    input = (,) <$> stacks <* P.endOfLine <* skipLine <* P.endOfLine <*> instructions
    stacks = do
      rows <- row `P.sepBy1` P.endOfLine
      maybe (fail "parse error") pure $ rowsToStacks rows
    row = sepByN " " 9 slot
    sepByN delim n p = do
      x <- p
      xs <- P.count (n - 1) (delim *> p)
      pure (x : xs)
    slot = occupiedSlot <|> unoccupiedSlot
    occupiedSlot = do
      _ <- "["
      c <- P.letter_ascii
      guard $ Char.isUpper c
      _ <- "]"
      pure $ Just c
    unoccupiedSlot = "   " $> Nothing
    skipLine = P.manyTill P.anyChar P.endOfLine
    instructions = instruction `P.sepBy1` P.endOfLine
    instruction = Instruction <$ "move " <*> nat <* " from " <*> nat <* " to " <*> nat
    nat = read <$> P.many1 P.digit

rowsToStacks =
  fmap (IntMap.fromList . zip [1 ..])
    . traverse (sequenceA . dropWhile Maybe.isNothing)
    . List.transpose

move stacks (Instruction number from to) =
  let s = stacks IntMap.! from
      t = stacks IntMap.! to
   in IntMap.insert from (drop number s) . IntMap.insert to (reverse (take number s) <> t) $ stacks

part1 (stacks, instructions) =
  let finalStacks = List.foldl' move stacks instructions
   in fmap head (IntMap.elems finalStacks)

move2 stacks (Instruction number from to) =
  let s = stacks IntMap.! from
      t = stacks IntMap.! to
   in IntMap.insert from (drop number s) . IntMap.insert to (take number s <> t) $ stacks

part2 (stacks, instructions) =
  let finalStacks = List.foldl' move2 stacks instructions
   in fmap head (IntMap.elems finalStacks)

main :: IO ()
main = do
  rawInput <- BS.readFile "input.txt"
  input <- either fail pure $ parse rawInput
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
