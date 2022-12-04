module Main where

import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as Char
import Data.Set qualified as Set

type Input = [([Item], [Item])]

newtype Item = Item {priority :: Int} deriving (Eq, Show, Ord)

parse :: BS.ByteString -> Either String Input
parse = P.parseOnly (input <* P.endOfLine)
  where
    input = rucksack `P.sepBy1` P.endOfLine
    rucksack = do
      items <- P.many1 item
      let n = length items `div` 2
          (xs, ys) = splitAt n items
      pure (xs, ys)
    item = mkItem <$> P.letter_ascii

mkItem x =
  let ord = Char.ord x
   in Item $
        if Char.isLower x
          then ord - Char.ord 'a' + 1
          else ord - Char.ord 'A' + 27

part1 = sum . fmap (priority . redundantItem)
  where
    redundantItem (xs, ys) =
      head $
        Set.elems $
          Set.fromList xs `Set.intersection` Set.fromList ys

chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

part2 = sum . fmap (priority . badge) . chunksOf 3
  where
    badge group =
      head $
        Set.elems $
          foldl1 Set.intersection [Set.fromList [z | zs <- [xs, ys], z <- zs] | (xs, ys) <- group]

main = do
  rawInput <- BS.readFile "input.txt"
  input <- either fail pure $ parse rawInput
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
