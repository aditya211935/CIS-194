{-# OPTIONS_GHC -Wall #-}
module Scrabble where

-- TODO: Using type instead of newtype or data gives weird orphan warnings. Look into it.
type Score = Int

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = 0

score :: Char -> Score
score x
  | x `elem` "AEILNORSTUaeilnorstu" = 1
  | x `elem` "DGdg" = 2
  | x `elem` "BCMPbcmp" = 3
  | x `elem` "FHVWYfhvwy" = 4
  | x `elem` "Kk" = 5
  | x `elem` "JXjx" = 8
  | x `elem` "QZqz" = 10
  | otherwise = 0

scoreString :: String -> Score
scoreString = foldr (\c acc -> score c <> acc) 0
