{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad.State
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
    input =
      (,)
        <$> stacks
        <* "\n"
        <* P.manyTill P.anyChar "\n"
        <* "\n"
        <*> instructions
    stacks = do
      rows <- row `P.sepBy1` "\n"
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
    instructions = instruction `P.sepBy1` "\n"
    instruction = Instruction <$ "move " <*> nat <* " from " <*> nat <* " to " <*> nat
    nat = read <$> P.many1 P.digit

rowsToStacks =
  fmap (IntMap.fromList . zip [1 ..])
    . traverse (sequenceA . dropWhile Maybe.isNothing)
    . List.transpose

pop from = do
  stacks <- get
  case stacks IntMap.! from of
    x : xs -> do
      put $ IntMap.insert from xs stacks
      pure $ Just x
    [] -> pure Nothing

push to x = do
  stacks <- get
  put $ IntMap.insert to (x : stacks IntMap.! to) stacks

execute (Instruction n from to) = replicateM_ n $ do
  mx <- pop from
  forM_ mx (push to)

tops = fmap head <$> IntMap.elems

part1 (stacks, instructions) = tops $ execState (traverse execute instructions) stacks

pop' from = do
  (stacks, cargo) <- get
  case stacks IntMap.! from of
    x : xs -> do
      put (IntMap.insert from xs stacks, x : cargo)
      pure $ Just x
    [] -> pure Nothing

push' to = do
  (stacks, cargo) <- get
  case cargo of
    x : xs -> put (IntMap.insert to (x : stacks IntMap.! to) stacks, xs)
    [] -> pure ()

execute' (Instruction n from to) = do
  replicateM_ n $ pop' from
  replicateM_ n $ push' to

part2 (stacks, instructions) =
  let (stacks', _) = execState (traverse execute' instructions) (stacks, [])
   in tops stacks'

main = do
  rawInput <- BS.readFile "input.txt"
  input <- either fail pure $ parse rawInput
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
