{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
-- module LogAnalysis where
module Main (main) where

import ExprT
import Parser
import MyParser (myParser)

eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
  Just expr -> Just $ eval expr
  Nothing -> Nothing

evalStr2 :: String -> Maybe Integer
evalStr2 str = case myParser str of
  Just expr -> Just $ eval expr
  Nothing -> Nothing

main :: IO()
main = do
  -- print (reify $ mul (add (lit 2) (lit 3)) (lit 4))
  -- print (gfbe 0 0 "(2)()")
  -- print (parseNum "124+33")
  print (evalStr2 "2*3+5") -- 16 (even though * is executed first in ghci, thus 11)
  print (evalStr2 "(2+3)*6") -- 30
  print (evalStr2 "2+3*") -- Nothing
  print (evalStr2 "(3*(22+33)*(((5*7+3))))") -- 8250
  -- print (eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)))
