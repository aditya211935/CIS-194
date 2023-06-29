-- Validate credit card numbers
-- https://www.seas.upenn.edu/~cis1940/spring13/hw/01-intro.pdf

toDigits    :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOtherPrivate :: [Integer] -> [Integer]
sumDigits :: [Integer] -> Integer
validate :: Integer -> Bool

toDigitsRev n
  | n < 10 = [n]
  | otherwise = (n `mod` 10):toDigitsRev(n `div` 10)

toDigits n = reverse(toDigitsRev(n))

doubleEveryOtherPrivate [] = []
doubleEveryOtherPrivate (x:[]) = [x]
doubleEveryOtherPrivate (x:y:zs) = x:(2*y):doubleEveryOtherPrivate(zs)

-- Flip the numbers to start doubling from second last.
-- Then flip again to get the output.
doubleEveryOther x = reverse(doubleEveryOtherPrivate(reverse(x)))

sumDigits [] = 0
sumDigits (x:ys)
  | x < 10 = x + sumDigits(ys)
  | otherwise = sumDigits(toDigits(x)) + sumDigits(ys)

validate n = sumDigits(doubleEveryOther(toDigits(n))) `mod` 10 == 0

main = print(validate 4012888888881881)
