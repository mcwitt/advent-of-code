{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.Bifunctor (second)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (toList)
import Data.Functor.Base (TreeF (..))
import Data.Functor.Foldable
import Data.List qualified as List
import Data.Tree (Tree (Node))
import Data.Tree qualified as Tree
import Data.Tree.Zipper qualified as Z

data Line
  = CdRoot
  | CdParent
  | Cd String
  | Dir String
  | Ls
  | File Int String
  deriving (Eq, Show)

type Input = [Line]

parse :: BS.ByteString -> Either String Input
parse = P.parseOnly input
  where
    input = line `P.sepBy` "\n"
    line =
      CdRoot <$ "$ cd /"
        <|> CdParent <$ "$ cd .."
        <|> Cd <$ "$ cd " <*> dirname
        <|> Dir <$ "dir " <*> dirname
        <|> Ls <$ "$ ls"
        <|> File <$> nat <* " " <*> filename
    dirname = P.many1 P.letter_ascii
    filename = P.many1 (P.letter_ascii <|> P.char '.')
    nat = read <$> P.many1 P.digit

type DirInfo = (String, [(String, Int)])

inferFiletree :: MonadFail m => [Line] -> m (Tree DirInfo)
inferFiletree = fmap Z.toTree . foldM (flip interpret) (Node ("/", []) [], [])
  where
    interpret = \case
      CdRoot -> pure . Z.root
      CdParent -> maybe (fail "root has no parent") pure . Z.parent
      Cd dirname -> pure . Z.child ((== dirname) . fst) (dirname, [])
      Dir dirname -> pure . Z.ensureChild ((== dirname) . fst) (dirname, [])
      File size name -> pure . Z.modify (second ((name, size) :))
      _ -> pure

totalSizes = cata $ \(NodeF (_, files) subdirs) ->
  let filesSize = sum . fmap snd $ files
      subdirsSize = sum . fmap Tree.rootLabel $ subdirs
   in Node (filesSize + subdirsSize) subdirs

part1 = sum . filter (<= 100000) . toList . totalSizes

part2 fileTree =
  let sizes = totalSizes fileTree
      total = 70000000
      needed = 30000000
      used = Tree.rootLabel sizes
      available = total - used
      minSize = needed - available
      Just ans = List.find (>= minSize) . List.sort . toList $ sizes
   in ans

main = do
  rawInput <- BS.readFile "input.txt"
  input <- either fail pure $ parse rawInput
  fileTree <- inferFiletree input
  putStrLn $ "Part 1: " <> show (part1 fileTree)
  putStrLn $ "Part 2: " <> show (part2 fileTree)
