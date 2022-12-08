module Data.Tree.Zipper where

import Data.List qualified as List
import Data.Tree (Tree (Node))
import Data.Tree qualified as Tree

data Crumb a = Crumb a [Tree a] [Tree a] deriving (Show)

type Zipper a = (Tree a, [Crumb a])

childAt :: Int -> Zipper a -> Zipper a
childAt i (Node x children, crumbs) =
  let (left, focus : right) = splitAt i children
   in (focus, Crumb x left right : crumbs)

child :: (a -> Bool) -> a -> Zipper a -> Zipper a
child p def z@(Node _ children, _) =
  maybe (insertChildAndVisit def z) (`childAt` z) $ List.findIndex (p . Tree.rootLabel) children

ensureChild :: (a -> Bool) -> a -> Zipper a -> Zipper a
ensureChild p def = unsafeParent . child p def

parent :: Zipper a -> Maybe (Zipper a)
parent (_, []) = Nothing
parent (focus, Crumb parent left right : crumbs) =
  Just (Node parent (left ++ [focus] ++ right), crumbs)

unsafeParent :: Zipper a -> Zipper a
unsafeParent z = let Just z' = parent z in z'

root :: Zipper a -> Zipper a
root ctxt = maybe ctxt root (parent ctxt)

insertChildAndVisit :: a -> Zipper a -> Zipper a
insertChildAndVisit x (Node y children, crumbs) =
  (Node x [], Crumb y [] children : crumbs)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x children, crumbs) = (Node (f x) children, crumbs)

toTree :: Zipper a -> Tree a
toTree ctxt = let (focus, _) = root ctxt in focus
