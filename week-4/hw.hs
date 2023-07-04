{-# OPTIONS_GHC -Wall #-}
-- module LogAnalysis where
module Main (main) where

import PrintTree
import Data.List

fun1 :: [Integer] -> Integer
fun1 = product . map (2 `subtract`) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/=1) . iterate (\a -> if even a then a `div` 2 else 3 * a + 1)

foldTree :: [a] -> Tree a
foldTree = foldr insert2 Leaf

getHeight :: Tree a -> Int
getHeight Leaf = -1
getHeight (Node h _ _ _) = h

insert2 :: a -> Tree a -> Tree a
insert2 x Leaf = Node 0 Leaf x Leaf
insert2 x (Node _ left val right) = case compare leftHeight rightHeight of
  GT -> Node (1 + rightHeight) left val tryRight
  _ -> Node (1 + leftHeight) tryLeft val right
  where
    tryLeft = insert2 x left
    tryRight = insert2 x right
    leftHeight = getHeight tryLeft
    rightHeight = getHeight tryRight

transformTree :: Tree a -> Tree Int
transformTree Leaf = Leaf
transformTree (Node h l v r) = Node h (transformTree l) (h) (transformTree r)

xor :: [Bool] -> Bool
xor = odd . length . foldr (\c acc -> if c == True then c:acc else acc) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\c acc -> f c:acc) []

foldlx :: (b -> a -> b) -> b -> [a] -> b
foldlx f z arr = foldr (flip f) z (reverse arr)

sieve :: Int -> [Int]
sieve n = [2 | n > 2] ++ map (\i -> 2*i+1) ([1..((n-1) `div` 2)] \\ filter (<=n) [x + y + 2 * x * y| x <- [1..n], y <- [x..n]])

main :: IO()
main = do
  print (sieve 7)
  -- print (foldTree "ABCDEFGHIJ")
  -- print (map (\c -> c+1) [1,2,3])
  -- print (map' (\c -> c+1) [1,2,3])
  -- print (xor [False, True, False])
  -- print (foldr (\c acc -> acc ++ [c]) "foo" "bar")
  -- print (foldl (\acc c -> acc ++ [c]) "foo" "bar")
  -- print (foldlx (\acc c -> acc ++ [c]) "foo" "bar")
  -- putStr (unlines $ printTree $ foldTree "ABCDEFGHIJ")
