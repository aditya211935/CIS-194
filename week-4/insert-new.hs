{-# OPTIONS_GHC -Wall #-}
-- module LogAnalysis where
module Main (main) where

import PrintTree

{-
  Improved version of insert that takes (log n) time to insert a node.
  It keeps track of the size of left and right subtree and chooses the subtree with the minimum size to insert a node.

  To use this, go to PrintTree.hs (or PrintTree2.hs for compact version) and change the type of Tree constructor to

  data Tree a = Leaf
            | Node (Int, Int) (Tree a) a (Tree a)
  deriving (Show, Eq)

  This basically stores a tuple denoting the size of left and right subtree instead of tree's Height.

  TODO: There's a better way to do this in haskell by adding another type parameter in Tree to be Tree a b =...
  where b denotes the metadata to be stored in the tree (in this case a tuple (Int, Int)). This will require
  refactoring other functions that work on Tree a inside PrintTree.hs, PrintTree2.hs and hw.hs
-}

foldTree :: [a] -> Tree a
foldTree = foldr insertNew Leaf

insertNew :: a -> Tree a -> Tree a
insertNew x Leaf = Node (0, 0) Leaf x Leaf
insertNew x (Node (lSz, rSz) left val right)
  | lSz > rSz = Node (lSz, 1+rSz) left val (insertNew x right)
  | otherwise = Node (1+lSz, rSz) (insertNew x left) val right

main :: IO()
main = do
  putStr (unlines (printTree $ foldTree ([0..6] :: [Integer])))
