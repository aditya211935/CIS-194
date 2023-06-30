{-# OPTIONS_GHC -Wall #-}

module Main (main) where

mapIntList :: (a -> b) -> [a] -> [b]
mapIntList _ [] = []
mapIntList f (x:xs) = (f x):(mapIntList f xs)

addOne :: Int -> Int
addOne a = a + 1

filterIntList :: (a -> Bool) -> [a] -> [a]
filterIntList _ [] = []
filterIntList f (x:xs)
  | f x = x : filterIntList f xs
  | otherwise = filterIntList f xs

greaterThan30 :: Int -> Bool
greaterThan30 a = a > 30

-- Given a f acc curr which returns a val, initialValue, and a List, it works same as Javascript's reduce function.
reduceIntList :: (a -> a -> a) -> a -> [a] -> a
reduceIntList _ acc [] = acc
reduceIntList f acc (x:xs) = reduceIntList f (f acc x) xs

sumOf2 :: Int -> Int -> Int
sumOf2 a b = a + b

main :: IO()
main = print (reduceIntList sumOf2 1 [1, 2, 3])
-- main = print (filterIntList greaterThan30 [10, 20, 30, 40, 50])
-- main = print (mapIntList addOne [0, 1, 2])
