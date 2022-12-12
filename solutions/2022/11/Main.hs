{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString.Char8 qualified as BS
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List qualified as List

type Input = [Monkey]

data Monkey = Monkey
  { items :: [Int],
    behavior :: Behavior,
    inspectCount :: Int
  }
  deriving (Show)

data Behavior = Behavior
  { operation :: Operation,
    testDivisibleBy :: Int,
    monkeyTrue :: Int,
    monkeyFalse :: Int
  }
  deriving (Show)

data Operation = Add Int | Multiply Int | MultBy2 | Square deriving (Show)

parse :: BS.ByteString -> Either String Input
parse = P.parseOnly input
  where
    input = monkey `P.sepBy` "\n"
    monkey =
      Monkey
        <$ "Monkey "
        <* nat
        <* ":\n"
        <* "  Starting items: "
        <*> nat
        `P.sepBy` ", "
        <* "\n"
        <*> behavior
        <*> pure 0
    behavior =
      Behavior
        <$ "  Operation: new = old "
        <*> (add <|> multiply <|> multBy2 <|> square)
        <* "\n"
        <* "  Test: divisible by "
        <*> nat
        <* "\n"
        <* "    If true: throw to monkey "
        <*> nat
        <* "\n"
        <* "    If false: throw to monkey "
        <*> nat
        <* "\n"
    add = Add <$ "+ " <*> nat
    multiply = Multiply <$ "* " <*> nat
    multBy2 = MultBy2 <$ "+ old"
    square = Square <$ "* old"
    nat = read <$> P.many1 P.digit

evalUpdate = \case
  Add x -> (+ x)
  Multiply x -> (* x)
  MultBy2 -> (* 2)
  Square -> (^ 2)

inspectItems updateWorry idx = do
  m@Monkey {..} <- gets (IntMap.! idx)
  modify $ IntMap.insert idx m {items = [], inspectCount = inspectCount + length items}
  forM_ items $ \initWorry -> do
    let (worry, throwTo) = processItem behavior initWorry
    modify $ IntMap.adjust (\m@Monkey {..} -> m {items = worry : items}) throwTo
  where
    processItem Behavior {..} initWorry =
      let worry = updateWorry operation initWorry
       in ( worry,
            if worry `divisibleBy` testDivisibleBy
              then monkeyTrue
              else monkeyFalse
          )
    divisibleBy n d = n `mod` d == 0

doRound f = do
  monkeys <- get
  forM_ [0 .. IntMap.size monkeys - 1] (inspectItems f)

part1 =
  product
    . take 2
    . reverse
    . List.sort
    . IntMap.elems
    . fmap inspectCount
    . execState (replicateM 20 (doRound updateWorry))
    . IntMap.fromList
    . zip [0 ..]
  where
    updateWorry operation = (`div` 3) . evalUpdate operation

part2 input =
  product
    . take 2
    . reverse
    . List.sort
    . IntMap.elems
    . fmap inspectCount
    . execState (replicateM 10000 (doRound updateWorry))
    . IntMap.fromList
    . zip [0 ..]
    $ input
  where
    updateWorry operation = (`mod` modulus) . evalUpdate operation
    modulus = product $ fmap (testDivisibleBy . behavior) input

main = do
  rawInput <- BS.readFile "input.txt"
  input <- either fail pure $ parse rawInput
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
