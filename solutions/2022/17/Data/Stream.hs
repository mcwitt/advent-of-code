module Data.Stream
  ( Stream (..),
    cycle,
    dropWhile,
    findCycle,
    findCycle',
    head,
    iterate,
    prepend,
    tail,
    take,
    zip,
    zipWith,
  )
where

import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NonEmpty
import Test.QuickCheck
import Prelude hiding (cycle, dropWhile, head, iterate, tail, take, zip, zipWith)

data Stream a = a :> Stream a deriving (Functor)

infixr 5 :>

head :: Stream a -> a
head (x :> _) = x

tail :: Stream a -> Stream a
tail (_ :> xs) = xs

iterate :: (a -> a) -> a -> Stream a
iterate f x = x :> iterate f (f x)

cycle :: NonEmpty a -> Stream a
cycle xs = foldr (:>) (cycle xs) xs

take :: Int -> Stream a -> [a]
take 0 _ = []
take n (x :> xs) = x : take (n - 1) xs

dropWhile :: (a -> Bool) -> Stream a -> Stream a
dropWhile p (x :> xs)
  | p x = dropWhile p xs
  | otherwise = x :> xs

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (x :> xs) (y :> ys) = f x y :> zipWith f xs ys

zip :: Stream a -> Stream b -> Stream (a, b)
zip = zipWith (,)

prepend :: Foldable f => f a -> Stream a -> Stream a
prepend = flip $ foldr (:>)

findCycle' :: (a -> a -> Bool) -> Stream a -> ([a], NonEmpty a)
findCycle' eq xs = go1 xs xs
  where
    go1 (t :> ts) (_ :> h :> hs)
      | t `eq` h = go2 xs ts []
      | otherwise = go1 ts hs
    go2 (t :> ts) (h :> hs) ys
      | t `eq` h = go3 ys t hs (t :| [])
      | otherwise = go2 ts hs (t : ys)
    go3 ys t (h :> hs) zs
      | t `eq` h = (reverse ys, NonEmpty.reverse zs)
      | otherwise = go3 ys t hs (h <| zs)

findCycle :: Eq a => Stream a -> ([a], NonEmpty a)
findCycle = findCycle' (==)

data PrefixAndCycle a = PrefixAndCycle [a] (NonEmpty a) deriving (Show)

instance (Eq a, Arbitrary a) => Arbitrary (PrefixAndCycle a) where
  arbitrary = do
    xs <- nub <$> listOf1 arbitrary
    let n = length xs
    i <- chooseInt (0, n - 1)
    let (ys, zs) = splitAt i xs
    return $ PrefixAndCycle ys (NonEmpty.fromList zs)

prop_inverse :: PrefixAndCycle Int -> Bool
prop_inverse (PrefixAndCycle xs ys) = findCycle (prepend xs (cycle ys)) == (xs, ys)
