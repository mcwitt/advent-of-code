{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString.Char8 qualified as BS
import Data.Functor (($>), (<&>))
import Data.Monoid (Alt (Alt, getAlt))

type Input = [(Move, Move)]

data Move = Rock | Paper | Scissors deriving (Eq, Show, Enum)

parse :: BS.ByteString -> Either String Input
parse = P.parseOnly (input <* P.endOfLine)
  where
    input = round_ `P.sepBy1` P.endOfLine
    round_ = (,) <$> theirMove <* P.space <*> ourMove
    theirMove = mkMove [('A', Rock), ('B', Paper), ('C', Scissors)]
    ourMove = mkMove [('X', Rock), ('Y', Paper), ('Z', Scissors)]
    mkMove = getAlt . foldMap (\(code, move) -> Alt (P.char code $> move))

data Outcome = Loss | Draw | Win deriving (Eq, Show)

outcome theirMove ourMove =
  if theirMove == ourMove
    then Draw
    else
      let d = fromEnum ourMove - fromEnum theirMove
       in if d == 1 || d == -2
            then Win
            else Loss

score theirMove ourMove =
  let outcomeScore = case outcome theirMove ourMove of
        Loss -> 0
        Draw -> 3
        Win -> 6
      moveScore = fromEnum ourMove + 1
   in outcomeScore + moveScore

part1 = sum . fmap (uncurry score)

winsAgainst = toEnum . (`mod` 3) . succ . fromEnum

losesTo = toEnum . (`mod` 3) . pred . fromEnum

part2 input =
  sum
    [ score theirMove (ourMove theirMove desiredOutcome)
      | (theirMove, desiredOutcome) <- correctedInput
    ]
  where
    correctedInput =
      input
        <&> fmap
          ( \case
              Rock -> Loss
              Paper -> Draw
              Scissors -> Win
          )

    ourMove theirMove desiredOutcome = case desiredOutcome of
      Loss -> losesTo theirMove
      Draw -> theirMove
      Win -> winsAgainst theirMove

main = do
  rawInput <- BS.readFile "input.txt"
  input <- either fail pure $ parse rawInput
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
