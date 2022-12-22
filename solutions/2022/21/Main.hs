{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString.Char8 qualified as BS
import Data.Char
import Data.Functor.Foldable
import Data.Map qualified as Map
import Data.Ratio

type Input = [(String, TreeInput Integer)]

data TreeInput a = NodeInput String Op String | LeafInput a deriving (Show)

data Op = Add | Sub | Mul | Div deriving (Show)

data BinTreeF a b r = NodeF a r r | LeafF b deriving (Show, Functor)

data BinTree a b = Node a (BinTree a b) (BinTree a b) | Leaf b deriving (Show)

type instance Base (BinTree a b) = BinTreeF a b

instance Recursive (BinTree a b) where
  project (Node x l r) = NodeF x l r
  project (Leaf x) = LeafF x

parse :: BS.ByteString -> Either String Input
parse = P.parseOnly input
  where
    input = monkey `P.sepBy1` "\n"
    monkey = node <|> leaf
    node = (,) <$> label <* ": " <*> (NodeInput <$> label <* " " <*> op <* " " <*> label)
    leaf = (,) <$> label <* ": " <*> (LeafInput <$> nat)
    label = P.many1 (P.satisfy (\c -> isLetter c && isLower c))
    nat = read <$> P.many1 P.digit
    op = Add <$ "+" <|> Sub <$ "-" <|> Mul <$ "*" <|> Div <$ "/"

makeTree :: (String -> TreeInput a) -> String -> BinTree Op a
makeTree f = go
  where
    go root = case f root of
      NodeInput left op right -> Node op (go left) (go right)
      LeafInput val -> Leaf val

part1 input = evalTree tree
  where
    tree = makeTree (byLabel Map.!) "root"
    byLabel = Map.fromList input

    evalTree = cata f
      where
        f (NodeF op l r) = eval op l r
        f (LeafF x) = x

        eval = \case
          Add -> (+)
          Sub -> (-)
          Mul -> (*)
          Div -> div

data Value a = L a a | S a

instance Num a => Num (Value a) where
  (S a) + (S b) = S (a + b)
  (L a b) + (S c) = L a (b + c)
  x + y = y + x

  S a - S b = S (a - b)
  L a b - S c = L a (b - c)
  S c - L a b = L a (c - b)

  S a * S b = S (a * b)
  L a b * S c = L (a * c) (b * c)
  x * y = y * x

  fromInteger = S . fromInteger

instance Fractional a => Fractional (Value a) where
  S a / S b = S (a / b)
  L a b / S c = L (a / c) (b / c)
  S _ / L _ _ = undefined -- this would require a rational variant, but the input does not require it

deriving instance Show a => Show (Value a)

part2 input
  | denominator solution == 1 = numerator solution
  where
    solution = solve x y
    x = evalTree left
    y = evalTree right
    Node _ left right = makeTree (byLabel Map.!) "root"
    byLabel = Map.fromList input'
    input' =
      fmap
        ( \case
            (lbl, _) | lbl == "humn" -> (lbl, LeafInput (L 1 0 :: Value (Ratio Integer)))
            (lbl, LeafInput x) -> (lbl, LeafInput (fromIntegral x))
            (lbl, NodeInput left op right) -> (lbl, NodeInput left op right)
        )
        input

    evalTree = cata f
      where
        f (NodeF op l r) = eval op l r
        f (LeafF x) = x

        eval = \case
          Add -> (+)
          Sub -> (-)
          Mul -> (*)
          Div -> (/)

    solve (L a b) (S c) = (b - c) / a
    solve x y = solve y x

main = do
  rawInput <- BS.readFile "input.txt"
  input <- either fail pure $ parse rawInput
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
