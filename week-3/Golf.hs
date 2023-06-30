{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.List

type Curr = Int
type Step = Int
type Index = Int

filterNth :: Curr -> Step -> [a] -> [a]
filterNth _ _ [] = []
filterNth curr step (x:xs)
  | curr `mod` step == 0 = x : rest
  | otherwise = rest
  where rest = filterNth (curr + 1) step xs

buildSkip :: [a] -> Index -> [[a]]
buildSkip a i
  | i <= length a = filterNth 1 i a:buildSkip a (i+1)
  | otherwise = []

-- TODO: make shorter. Probably use list comprehension
skips :: [a] -> [[a]]
skips [] = []
skips a = buildSkip a 1

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:ds)
  | b > a && b > c = b : localMaxima (b:c:ds)
  | otherwise = localMaxima (b:c:ds)
localMaxima _ = []

type Count = Integer
type Value = Integer
data Elem = Elem Count Value deriving (Show, Eq)

f :: Integer -> [Elem] -> [Elem]
f curr [] = [Elem 1 curr]
f curr (Elem cnt val:xs)
  | val == curr = (Elem (cnt + 1) val):xs
  | otherwise = (Elem 1 curr):(Elem cnt val):xs

sc :: [Integer] -> [Elem]
sc a = foldr f [] (sort a)

row :: [Elem] -> String
row [] = "\n"
row (Elem cnt _:xs)
  | cnt == 0 = " " ++ row xs
  | otherwise = "*" ++ row xs

sub1 :: [Elem] -> [Elem]
sub1 [] = []
sub1 (Elem cnt val: xs)
  | cnt == 0 = Elem cnt val : sub1 xs
  | otherwise = Elem (cnt - 1) val : sub1 xs

checkAll :: [Elem] -> Bool
checkAll [] = False
checkAll (Elem cnt _:xs) = (cnt > 0) || checkAll xs

cons :: [Elem] -> String
cons a
  | (checkAll a) == True = (cons (sub1 a)) ++ (row a)
  | otherwise = ""

s2 :: [Integer] -> [Elem]
s2 a = sub1 (sc (a ++ [0,1,2,3,4,5,6,7,8,9]))

histogram :: [Integer] -> String
histogram a = cons (s2 a) ++ "==========\n0123456789\n"

main :: IO()
main = do
  -- let val = (skips []) :: [[Int]]
  putStr (histogram [1,4,5,4,6,6,3,4,2,4,9])
  -- putStrLn (row $ s2 [5,4,6,5,4,6,7,6,6])
