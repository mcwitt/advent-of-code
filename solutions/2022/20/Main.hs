module Main where

import Control.Arrow (Arrow ((&&&)), (>>>))
import Data.Foldable (foldl')
import Data.Maybe (fromJust)

main = interact solve

solve = parse >>> (part1 &&& part2) >>> show

type Input = [Int]

parse :: String -> Input
parse = lines >>> map read

data Zipper a = Z [a] a [a] deriving (Show)

zipper :: [a] -> Zipper a
zipper (x : xs) = Z [] x xs

zipperLength :: Zipper a -> Int
zipperLength (Z sx _ xs) = 1 + length sx + length xs

focus :: Zipper a -> a
focus (Z _ x _) = x

moveRight :: Zipper a -> Maybe (Zipper a)
moveRight (Z sx x (x' : xs)) = Just $ Z (x : sx) x' xs
moveRight (Z _ _ []) = Nothing

moveLeft :: Zipper a -> Maybe (Zipper a)
moveLeft (Z (x' : sx) x xs) = Just $ Z sx x' (x : xs)
moveLeft (Z [] _ _) = Nothing

rewind :: Zipper a -> Zipper a
rewind = exhaust moveLeft
  where
    exhaust f x = maybe x (exhaust f) (f x)

findRight :: (a -> Bool) -> Zipper a -> Maybe (Zipper a)
findRight p z
  | p (focus z) = Just z
  | otherwise = moveRight z >>= findRight p

find p = fromJust . findRight p . rewind

moveRightC (Z sx x (x' : xs)) = Z (x : sx) x' xs
moveRightC (Z sx x []) = let x' : xs = reverse sx in Z [x] x' xs

dragRightC (Z sx x (x' : xs)) = Z (x' : sx) x xs
dragRightC (Z sx x []) = let x' : xs = reverse sx in Z [x'] x xs

dragLeftC (Z (x' : sx) x xs) = Z sx x (x' : xs)
dragLeftC (Z [] x xs) = let x' : sx = reverse xs in Z sx x [x']

infixl 1 ><

f >< n = iterate f >>> (!! n)

dragC n
  | n >= 0 = dragRightC >< n
  | otherwise = dragLeftC >< (-n)

minOn f x y
  | f x <= f y = x
  | otherwise = y

modC n d = minOn abs m (m - d)
  where
    m = mod n d

infixl 0 #

(#) = flip ($)

coordinates z =
  z
    # find ((== 0) . snd)
    # iterate (moveRightC >< 1000 `mod` n)
    # tail
    # take 3
    # map (snd . focus)
  where
    n = zipperLength z

mix indexedNumbers z = foldl' go z indexedNumbers
  where
    go z t@(_, k) = find (== t) z # dragC (k `modC` (n - 1))
    n = zipperLength z

part1 numbers =
  mix indexedNumbers (zipper indexedNumbers)
    # coordinates
    # sum
  where
    indexedNumbers = zip [1 ..] numbers

part2 numbers =
  (mix indexedNumbers >< 10) (zipper indexedNumbers)
    # coordinates
    # sum
  where
    numbers' = map (* 811589153) numbers
    indexedNumbers = zip [1 ..] numbers'
