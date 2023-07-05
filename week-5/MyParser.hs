{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
-- module LogAnalysis where
module MyParser where

import ExprT
import Data.Char (isDigit)
import Text.Read (readMaybe)

{-
  My hacky implementation of the parser.
  An expression can begin from ( or a number.
  If it begins from ( do the following
    1. Take out left part, the expression, and right part. Call them l, expr, r.
    2. if there's nothing after l, then r and expr will be empty string. Parse l again after removing the brackets.
    3. Otherwise parse l and r. If both of them are Just, then combine everything based on expr.

  If it begins with a number then
    1. Take l, expr, r.
    2. If there's no expr and r, then simply parse l and return Lit l.
    3. Otherwise parse l and r separately. If they are Just, then combine everything based on expr.
-}

prsChk1 :: Maybe String -> Maybe String -> String -> Maybe (String, String, String)
prsChk1 Nothing _ _ = Nothing
prsChk1 _ Nothing _ = Nothing
prsChk1 (Just l) (Just opr) r = Just (l, opr, r)

prsChk2 :: Maybe ExprT -> Maybe ExprT -> Maybe (ExprT, ExprT)
prsChk2 Nothing _ = Nothing
prsChk2 _ Nothing = Nothing
prsChk2 (Just pL) (Just pR) = Just (pL, pR)

getLength :: Maybe String -> Int
getLength Nothing = 0
getLength (Just x) = length x

myParser :: String -> Maybe ExprT
-- Will never be used
myParser [] = Nothing
myParser ('(':xs) = case fin of
  Nothing -> Nothing
  Just (l, o, r) -> case o of
    "" -> case pL of
      Nothing -> Nothing
      (Just lExp) -> Just lExp
    "+" -> case prsChk2 pL pR of
      Nothing -> Nothing
      (Just (pL1, pR2)) -> Just $ Add pL1 pR2
    "*" -> case prsChk2 pL pR of
      Nothing -> Nothing
      (Just (pL1, pR2)) -> Just $ Mul pL1 pR2
    _ -> Nothing
    where
      pL = myParser $ removeBrackets l
      pR = myParser r
  where
    lTmp = gfbe 0 0 ('(':xs)
    opr = parseOpr (getLength lTmp) ('(':xs)
    rest = drop (getLength lTmp + 1) ('(':xs)
    fin = prsChk1 lTmp opr rest

myParser xs = case fin of
  Nothing -> Nothing
  (Just (n, o, r)) -> case o of
    "" -> Just $ Lit (toInteger n)
    "+" -> case pR of
      Nothing -> Nothing
      (Just rExp) -> Just $ Add (Lit (toInteger n)) rExp
    "*" -> case pR of
      Nothing -> Nothing
      (Just rExp) -> Just $ Mul (Lit (toInteger n)) rExp
    _ -> Nothing
    where
      pR = myParser r
  where
    lTmp = parseNum xs
    opr = parseOpr (getLength lTmp) xs
    rest = drop (getLength lTmp + 1) xs
    fin = prsChk3 lTmp opr rest

prsChk3 :: Maybe String -> Maybe String -> String -> Maybe (Int, String, String)
prsChk3 Nothing _ _ = Nothing
prsChk3 _ Nothing _ = Nothing
prsChk3 (Just numStr) (Just opr) r = case num of
  Nothing -> Nothing
  (Just x) -> Just (x, opr, r)
  where
    num = readMaybe numStr :: Maybe Int


-- AppendMaybeString
ams :: Char -> Maybe String -> Maybe String
ams s ms = case ms of
  Just x -> Just (s:x)
  Nothing -> Nothing

-- GetFirstBracketedExpression
-- Remove first layer of bracket and return the expression. Eg ((2+3))+(5+3) -> (2+3). (2+3) -> 2+3
gfbe :: Int -> Int -> String -> Maybe String
gfbe lc rc [] = case lc == rc of
  True -> Just ""
  False -> Nothing
gfbe lc rc (x:xs) = case (lc == rc) && lc /= 0 of
  True -> Just ""
  False -> case x of
    '(' -> ams '(' (gfbe (lc+1) rc xs)
    ')' -> ams ')' (gfbe lc (rc+1) xs)
    _ -> ams x (gfbe lc rc xs)

-- Removes first and last element from list. Eg. (2+3) -> 2+3.
-- Doesn't do much checking. Ensure correct value is provided in the arg.
removeBrackets :: String -> String
removeBrackets = tail . init

-- Returns the first string that contains a number. eg. 123+444 -> 123
parseNum :: String -> Maybe String
parseNum x = case length num of
  0 -> Nothing
  _ -> Just num
  where
    num = takeWhile (\c -> isDigit c) x

parseOpr :: Int -> String -> Maybe String
parseOpr off str = case (off + 1) <= length str of
  True -> case (str !! off) of
    '*' -> Just "*"
    '+' -> Just "+"
    _ -> Nothing
  False -> Just ""

-- main :: IO()
-- main = do
  -- print (gfbe 0 0 "(2)()")
  -- print (parseNum "124+33")
  -- print (evalStr2 "2*3+5") -- 17
  -- print (evalStr2 "(2+3)*6") -- 25
  -- print (evalStr2 "2+3*") -- Nothing
  -- print (evalStr2 "(3*(22+33)*(((5*7+3))))") -- 5
  -- print (eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)))
