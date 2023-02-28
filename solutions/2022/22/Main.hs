{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Arrow ((&&&), (>>>))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Array
import Data.Char (isDigit)
import Data.Functor (($>))
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Mod
import Linear (V2 (..))
import Text.ParserCombinators.ReadP hiding (get, optional)

main = interact solve

solve = read >>> (part1 &&& part2) >>> show

data Input = Input {grid :: Grid Tile, dirs :: Directions} deriving (Show)

type Directions = (Int, [(Turn, Int)])

type Grid a = Array (V2 Int) (Maybe a)

data Tile = Open | Wall deriving (Show, Eq)

data Turn = L | R deriving (Show)

mkGrid :: [[Maybe a]] -> Grid a
mkGrid xss =
  accumArray
    (const id)
    Nothing
    (V2 1 1, V2 rows cols)
    [ (V2 r c, x)
      | (r, xs) <- zip [1 ..] xss,
        (c, x) <- zip [1 ..] xs
    ]
  where
    rows = length xss
    cols = maximum $ map length xss

instance Read Input where
  readsPrec _ = readP_to_S parser
    where
      parser = Input <$> grid <* count 2 (char '\n') <*> directions
      grid = mkGrid <$> sepBy1 row (char '\n')
      row = many1 tile
      tile =
        asum
          [ char ' ' $> Nothing,
            char '.' $> Just Open,
            char '#' $> Just Wall
          ]
      directions = (,) <$> nat <*> many1 ((,) <$> turn <*> nat)
      nat = read <$> many1 (satisfy isDigit)
      turn =
        asum
          [ char 'L' $> L,
            char 'R' $> R
          ]

newtype Logo a = Logo {unLogo :: ReaderT (Grid Tile) (StateT (Pos, Hdg) Maybe) a}
  deriving
    ( Functor,
      Applicative,
      Alternative,
      Monad,
      MonadReader (Grid Tile),
      MonadError (),
      MonadState (Pos, Hdg)
    )

type Pos = V2 Int

type Hdg = Mod 4

unsafeMove :: Logo ()
unsafeMove = do
  grid <- ask
  (pos, hdg) <- get
  let newPos =
        pos + case hdg of
          0 -> V2 0 1
          1 -> V2 1 0
          2 -> V2 0 (-1)
          3 -> V2 (-1) 0
  put (newPos, hdg)

currTile :: Logo (Maybe Tile)
currTile = do
  grid <- ask
  (pos, _) <- get
  return $ join (grid !? pos)

whileReversed :: Logo a -> Logo a
whileReversed f = do uTurn; x <- f; uTurn; return x
  where
    uTurn = replicateM_ 2 (turn R)

move :: Logo ()
move = do
  unsafeMove
  t <- currTile
  case t of
    Nothing -> do
      whileReversed $ untilM_ unsafeMove (isJust <$> currTile)
      unsafeMove
      t <- currTile
      guard (t == Just Open)
    Just Open -> return ()
    Just Wall -> throwError ()

infixl 9 !?

(!?) :: Ix i => Array i e -> i -> Maybe e
(!?) arr i = if inRange (bounds arr) i then Just (arr ! i) else Nothing

untilM_ :: Monad m => m a -> m Bool -> m ()
untilM_ act p = loop
  where
    loop = do
      act
      x <- p
      when x loop

turn :: Turn -> Logo ()
turn t = do
  modify
    ( \(pos, hdg) ->
        ( pos,
          case t of
            L -> hdg - 1
            R -> hdg + 1
        )
    )

startCol :: Grid Tile -> Int
startCol = (+ 1) . length . takeWhile isNothing . elems

rawInput = readFile "test.txt"

input :: IO Input
input = read <$> rawInput

execLogo grid = flip execStateT (V2 1 (startCol grid), 0) . flip runReaderT grid . unLogo

part1 (Input grid (d, directions)) = 1000 * r + 4 * c + fromIntegral (unMod hdg)
  where
    (V2 r c, hdg) = fromJust $ execLogo grid prog
    prog = (:) <$> replicateM d step <*> mapM instr directions
    instr (t, d) = turn t *> replicateM d step
    step = optional move *> get

part2 :: Input -> Int
part2 = undefined
