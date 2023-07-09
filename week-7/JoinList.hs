{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module JoinList where

import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (tag l <> tag r) l r

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
-- Check index out of bounds
indexJ i jl | getSize (size (tag jl)) <= i || i < 0 = Nothing
-- Should never happen
indexJ _ Empty = Nothing
-- If it's Single then index should be zero. Overlaps with above condition for i < 0
indexJ i (Single _ val)
  | i == 0 = Just val
  | otherwise = Nothing
indexJ i (Append _ l r)
  | i < leftSz = indexJ i l
  | otherwise = indexJ (i - leftSz) r
  where
    leftSz = getSize (size (tag l))

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ d jl
-- This check is not redundant. It saves from applying checks separately to Single and Append.
  | getSize (size (tag jl)) <= d = Empty
  | d <= 0 = jl
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ d (Append _ l r)
  | d >= leftSz = Empty +++ dropJ (d - leftSz) r
  | otherwise = dropJ d l +++ r
  where
    leftSz = getSize (size (tag l))

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ t jl
  | getSize (size (tag jl)) <= t = jl
  | t <= 0 = Empty
takeJ _ Empty = Empty
takeJ _ (Single m a) = Single m a
takeJ t (Append _ l r)
  | t >= leftSz = l +++ takeJ (t - leftSz) r
  | otherwise = takeJ t l +++ Empty
  where
    leftSz = getSize (size (tag l))

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where
  toString jl = case jl of
    Empty -> ""
    Single _ a -> a
    Append _ l r -> toString l ++ toString r

  fromString = fromList . lines

  line = indexJ

  -- TODO: This will progressively unbalance the tree with every edit because it brings the line at n up to the
  -- top, merges it with left subtree and then with the right subtree.
  -- We can optimize it by not using takeJ, dropJ, and just replacing the line in its original place.
  replaceLine n _ jl | n < 0 || n >= getSize (size (tag jl)) = jl
  replaceLine n s jl = takeJ n jl +++ Single (scoreString s, 1) s +++ dropJ (n+1) jl

  numLines jl = getSize (size (tag jl))

  value Empty = 0
  value (Single (x, _) _) = x
  value (Append (x, _) _ _) = x

fromList :: [String] -> (JoinList (Score, Size) String)
fromList [] = Empty
fromList (x:[]) = Single (scoreString x, 1) x
fromList list = case even len of
  True -> fromList (take (len `div` 2) list) +++ fromList (drop (len `div` 2) list)
  False -> fromList (take ((len `div` 2) + 1) list) +++ fromList (drop ((len `div` 2) + 1) list)
  where
    len = length list
