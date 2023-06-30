{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.List

skips :: [a] -> [[a]]
skips a = [[fst y | y <- zip a [1..length a], snd y `mod` x == 0] | x <- [1..length a]]

-- Not really shorter than the more straightforward solution
localMaxima :: [Integer] -> [Integer]
localMaxima = foldr (r . take 3 . reverse) [] . inits
  where
    r (a:b:c:_) acc
      | b > a && b > c = b:acc
      | otherwise = acc
    r _ acc = acc

localMaxima2 :: [Integer] -> [Integer]
localMaxima2 = map getMid . filter check . filter ((>=3) . length) . map (take 3 . reverse) . inits
  where getMid (_:b:_) = b
        getMid _ = 0
        check (a:b:c:_) = b > a && b > c
        check _ = False

-- Short but messy histogram
histogram :: [Integer] -> String
histogram a = unlines (foldr (\c acc -> [if x - length acc <= 0 then ' ' else '*' | x <- c]:acc) [] (replicate (maximum cnt) cnt)) ++ "==========\n0123456789\n"
  where
    cnt = map (\n -> length n - 1) $ group $ sort $ a ++ [0..9]

-- Takes offset, list of counts and returns a correct row by subtracting offset from list of counts.
buildRow :: Int -> [Int] -> String
buildRow off xs = [if x - off <= 0 then ' ' else '*' | x <- xs]

reducerFunc :: [Int] -> [String] -> [String]
reducerFunc curr acc = buildRow (length acc) curr : acc

-- Longer but a bit cleaner
histogram2 :: [Integer] -> String
histogram2 a = unlines (foldr reducerFunc [] (replicate (maximum cnt) cnt)) ++ "==========\n0123456789\n"
  where cnt = map (\n -> length n - 1) $ group $ sort $ a ++ [0..9]

main :: IO()
main = do
  putStr (histogram2 [0..9])
