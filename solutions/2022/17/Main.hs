{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Arrow ((&&&), (>>>))
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Stream qualified as Stream
import Linear (V2 (V2))

main = interact solve

solve = parse >>> (part1 &&& part2) >>> show

-- solve = parse >>> part2 >>> show

data LR = L | R deriving (Show, Eq)

parse =
  lines
    >>> head
    >>> map
      ( \case
          '<' -> L
          '>' -> R
          x -> error $ "unexpected char: " ++ [x]
      )

type Sprite = [V2 Int]

at :: Sprite -> V2 Int -> Sprite
at s p = map (+ p) s

data World = World
  { occupied :: Set (V2 Int),
    pos :: V2 Int,
    jets :: [(Int, LR)],
    shapes :: [Sprite],
    stopped :: Int,
    baseHeight :: Int
  }
  deriving (Eq)

width :: Int
width = 7

render :: World -> String
render w@World {..} =
  unlines
    . reverse
    $ [ [pixel (V2 x y) | x <- [0 .. width - 1]]
        | y <- [0 .. ymax]
      ]
  where
    ymax = max (towerHeight w) (maximum . map (\(V2 _ y) -> y) $ head shapes `at` pos)
    pixel p
      | Set.member p occupied = '#'
      | p `elem` head shapes `at` pos = '@'
      | otherwise = '.'

initShapes =
  cycle
    [ [V2 0 0, V2 1 0, V2 2 0, V2 3 0],
      [V2 1 0, V2 0 1, V2 1 1, V2 2 1, V2 1 2],
      [V2 0 0, V2 1 0, V2 2 0, V2 2 1, V2 2 2],
      [V2 0 0, V2 0 1, V2 0 2, V2 0 3],
      [V2 0 0, V2 0 1, V2 1 0, V2 1 1]
    ]

initWorld jets =
  World
    { occupied = Set.empty,
      pos = V2 2 3,
      jets = cycle $ zip [0 ..] jets,
      shapes = cycle initShapes,
      stopped = 0,
      baseHeight = 0
    }

propagate = moveLR >>> moveDown

moveLR w@World {jets = (_, jet) : jets'} =
  try
    ( move $ case jet of
        L -> V2 (-1) 0
        R -> V2 1 0
    )
    (w {jets = jets'})

try :: (a -> Maybe a) -> a -> a
try f a = fromMaybe a (f a)

moveDown w@World {..} = fromMaybe (simplify w') $ move (V2 0 (-1)) w
  where
    w' =
      w
        { occupied = Set.union occupied . Set.fromList $ locatedShape,
          pos = V2 2 (towerHeight w' + 3),
          shapes = shapes',
          stopped = stopped + 1
        }
    locatedShape = shape `at` pos
    shape : shapes' = shapes

simplify :: World -> World
simplify w@World {..} = case paths of
  [] -> w
  ps ->
    let y0 = maximum $ map (minimum . map (\(V2 _ y) -> y)) ps
     in w
          { occupied = Set.fromList [V2 x y' | V2 x y <- Set.elems occupied, let y' = y - y0, y' >= 0],
            pos = let V2 x y = pos in V2 x (y - y0),
            baseHeight = baseHeight + y0
          }
  where
    starts = [[V2 x y] | V2 x y <- Set.toList occupied, x == 0]
    f (V2 x y : ps) =
      [ V2 x' y' : V2 x y : ps
        | let x' = x + 1,
          y' <- [y - 1, y, y + 1],
          V2 x' y' `Set.member` occupied
      ]
    paths = iterate (>>= f) starts !! (width - 1)

towerHeight World {..}
  | not (Set.null occupied) = Set.elems occupied & map (\(V2 _ y) -> y) & maximum & (+ 1)
  | otherwise = 0

height w@World {..} = baseHeight + towerHeight w

move :: V2 Int -> World -> Maybe World
move d w@World {..}
  | all (\p -> inBounds p && Set.notMember p occupied) (head shapes `at` pos') = Just $ w {pos = pos'}
  | otherwise = Nothing
  where
    pos' = pos + d
    inBounds (V2 x y) = 0 <= x && x < width && 0 <= y

worlds = Stream.iterate propagate . initWorld

part1 = worlds >>> Stream.dropWhile (stopped >>> (< 2022)) >>> Stream.head >>> height

part2 input = cycles * dHeight + heightRemaining
  where
    finalStopped = 1000000000000
    (_, c :| cs) = input & worlds & Stream.findCycle' (\x y -> proj x == proj y)
    proj World {..} = (occupied, pos, head jets, head shapes)
    dStopped = stopped (propagate $ last cs) - stopped c
    dHeight = height (propagate $ last cs) - height c
    (cycles, stoppedRemaining) = divMod (finalStopped - stopped c) dStopped
    heightRemaining = Stream.iterate propagate c & Stream.dropWhile (stopped >>> (< stoppedRemaining + stopped c)) & Stream.head & height
