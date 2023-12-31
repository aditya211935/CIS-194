{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.List
import Data.Maybe (fromMaybe)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Builds fibonacci series in reverse. Eg.
-- 8, 5, 3, 2, 1, 1, 0
fibL :: [Integer] -> [Integer]
fibL [] = [0]
fibL [_] = [1, 0]
fibL (x1:x2:xs) = (x1+x2):x1:x2:xs

fib2 :: Integer -> [Integer]
fib2 0 = [0]
fib2 1 = [0, 1]
fib2 n = reverse $ foldr (\_ acc -> fibL acc) [] [1..n]

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons val rest) = val:streamToList rest

instance Show a => Show (Stream a) where
  show = show . take 10 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x rest) = Cons (f x) (streamMap f rest)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f z = Cons z (streamFromSeed f (f z))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

getRules :: Integer -> Integer
getRules x = fst (fromMaybe (0, 1) (find (\(_, c) -> x `mod` c == 0) (reverse (zip [0..] (takeWhile (<= x) (iterate (*2) 1))))))

rules :: Stream Integer
rules = streamMap getRules (streamFromSeed (+1) 1)

-- Helper functions for exercise 6
-- Some operations to visualize how series are multiplied and divided.
addPrint :: Stream String -> Stream String -> Stream String
addPrint (Cons a0 a') (Cons b0 b') = Cons ("(" ++ a0 ++ "+" ++ b0 ++ ")") (addPrint a' b')

-- Try this with 3 terms (change "take 10" to 3 above in show function) for it to make sense. Any larger and it'll be messy. Eg.
-- print (mulPrint xPrint yPrint)
-- NOTE: To change numbers of terms to display, change the argument to `take` function in `show` function above (probably line 28).
mulPrint :: Stream String -> Stream String -> Stream String
mulPrint (Cons a0 a') (Cons b0 b') = Cons ("(" ++ a0 ++ "*" ++ b0 ++ ")") (addPrint (mulPrint (streamRepeat a0) b') (mulPrint a' (Cons b0 b')))

subPrint :: Stream String -> Stream String -> Stream String
subPrint (Cons a0 a') (Cons b0 b') = Cons ("(" ++ a0 ++ "-" ++ b0 ++ ")") (subPrint a' b')

-- This is a little crazy. Even just printing 4 terms of the series will lead to a huge 10 line result 🫠. Eg.
-- print (divPrint (streamRepeat "1") (subPrint (streamRepeat "1") xPrint))
-- The above represents (1/1-x) which gives coefficients 1,1,1,1...
divPrint :: Stream String -> Stream String -> Stream String
divPrint (Cons a0 a') (Cons b0 b') = Cons ("(" ++ a0 ++ "/" ++ b0 ++ ")") (mulPrint (divPrint (streamRepeat "1") (streamRepeat b0)) (subPrint a' (mulPrint (divPrint (Cons a0 a') (Cons b0 b')) b')))

xPrint :: Stream String
xPrint = streamFromSeed inc "a0"

yPrint :: Stream String
yPrint = streamFromSeed inc "b0"

inc :: String -> String
inc (v:rs) = v:show ((read rs :: Int) + 1)

-- Exercise 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap (\c -> c * (-1))
  (+) (Cons a0 a') (Cons b0 b') = Cons (a0 + b0) (a' + b')
  (*) (Cons a0 a') (Cons b0 b') = Cons (a0 * b0) (fromInteger a0 * b' + a' * Cons b0 b')

instance Fractional (Stream Integer) where
  (/) (Cons a0 a') (Cons b0 b') = Cons (a0 `div` b0) ((1 / fromInteger b0) * (a' - (Cons a0 a' / Cons b0 b') * b'))

fib3 :: Stream Integer
-- 🤯🤯🤯
fib3 = x / (1 - x - x^(2 :: Integer))

-- (0,0), (0,1), (1,0), (1,1)
data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
  (*) (Matrix a00 a01 a10 a11) (Matrix b00 b01 b10 b11) = Matrix (a00*b00 + a01*b01) (a00*b10 + a01*b11) (a10*b00 + a11*b01) (a10*b10 + a11*b11)


fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = getNum $ Matrix 1 1 1 0 ^ n
  where getNum (Matrix _ a01 _ _) = a01

-- To force fibonacci number to be evaluated.
lazyForce :: Integer -> Bool
lazyForce n = n > 9999999999

main :: IO()
main = do
  -- print (inc "a0")
  print (lazyForce (fib4 1234567))
  -- print (fib3)
  -- print (divPrint (streamRepeat "1") (subPrint (streamRepeat "1") xPrint))
  -- print (streamMap (\c -> c + 1) $ streamRepeat 11)
  -- print (streamFromSeed (\c -> c * 2) 1)
  -- print (nats)
  -- print (rules)
  -- print ([fib x| x <- [0..38]])


-- ( (1/(1-a0)) * (1-((1/(1-a0)) * (1-a1))) )
